--
-- Uwe R. Zimmer, Australia 2015
--

with Ada.Interrupts.Names;                        use Ada.Interrupts.Names;
with Ada.Real_Time;                               use Ada.Real_Time;
with Ada.Synchronous_Task_Control;                use Ada.Synchronous_Task_Control;
with ANU_Base_Board;                              use ANU_Base_Board;
with ANU_Base_Board.Config;                       use ANU_Base_Board.Config;
with ANU_Base_Board.Com_Interface;                use ANU_Base_Board.Com_Interface;
with ANU_Base_Board.LED_Interface;                use ANU_Base_Board.LED_Interface;
with Discovery_Board;                             use Discovery_Board;
with Discovery_Board.Button;                      use Discovery_Board.Button;
with Discovery_Board.Config;                      use Discovery_Board.Config;
with Discovery_Board.LED_Interface;               use Discovery_Board.LED_Interface;
with STM32F4;                                     use type STM32F4.Bit, STM32F4.Bits_32;
use STM32F4;
with STM32F4.General_purpose_IOs;                 use STM32F4.General_purpose_IOs;
with STM32F4.General_purpose_IOs.Ops;             use STM32F4.General_purpose_IOs.Ops;
with STM32F4.Interrupts_and_Events;               use STM32F4.Interrupts_and_Events;
with STM32F4.Interrupts_and_Events.Ops;           use STM32F4.Interrupts_and_Events.Ops;
with STM32F4.System_configuration_controller.Ops; use STM32F4.System_configuration_controller.Ops;
with STM32F4.Random_number_generator.Ops;         use STM32F4.Random_number_generator.Ops;
with STM32F4.Reset_and_clock_control.Ops;         use STM32F4.Reset_and_clock_control.Ops;
with System;                                      use System;

package body Generator_Controllers is

   -- Parameters about the sine wave.
   Signal_Frequency : constant Integer := 2;
   Sample_Frequency : constant Integer := 16;

   -- Constants.
   PI : constant Float := 3.1415;
   PI_D : constant Float := PI * 2.0;
   type Sine_Taylor_Level is range 1 .. 9;
   Sine_Taylor_Parameters : constant array (Sine_Taylor_Level) of Float := (-1.6666666666667E-01,
                                                                            8.3333333333333E-03,
                                                                            -1.9841269841270E-04,
                                                                            2.7557319223986E-06,
                                                                            -2.5052108385442E-08,
                                                                            1.6059043836822E-10,
                                                                            -7.6471637318198E-13,
                                                                            2.8114572543455E-15,
                                                                            -8.2206352466243E-18);

   -- Sample parameters.
   Sample_X_Slide : constant Float := 1.0 / Float (Sample_Frequency);
   type Sample_Range is range 1 .. Sample_Frequency;

   -- Signal parameters.
   System_Start : constant Time := Clock;
   Sine_Single_Period_Value : constant Integer := 1000 / Signal_Frequency;
   Sine_Single_Period : constant Time_Span := Milliseconds (Sine_Single_Period_Value);
   Half_Single_Period : constant Time_Span := Sine_Single_Period / 2;
   Calc_Signal_Period : constant Time_Span := Milliseconds (Sine_Single_Period_Value / Sample_Frequency);
   Current_Cycle_Start : Time := System_Start;
   Next_Cycle_Should_Start : Time := System_Start;
   Next_Cycle_Start : Time := System_Start;

   -- Sine calcuation.
   function Sine_Level (Base_X : Float; X_Square : Float; Level : Sine_Taylor_Level) return Float is
      Current_Base_X : constant Float := Base_X * X_Square;
      Current_Level_Result : constant Float := Current_Base_X * Sine_Taylor_Parameters (Level);
   begin
      if Level = Sine_Taylor_Level'Last then
         return Current_Level_Result;
      else
         return Current_Level_Result + Sine_Level (Base_X => Current_Base_X,
                                                   X_Square => X_Square,
                                                   Level => Sine_Taylor_Level'Succ (Level));
      end if;
   end Sine_Level;

   function Sine (X : Float) return Float is
      Real_X : constant Float := PI_D * X;
   begin
      return Real_X + Sine_Level (Base_X => Real_X,
                                  X_Square => Real_X * Real_X,
                                  Level => Sine_Taylor_Level'First);
   end Sine;

   Com_Ports_Semaphore : array (Com_Ports) of Suspension_Object;

   LED_Pattern : constant array (Com_Ports) of Discovery_Board.LEDs  := (Orange, Red, Blue, Green);

   protected Com_Port_Reader with Interrupt_Priority => Interrupt_Priority'Last is
   private
      procedure Interrupt_Handler with Attach_Handler => EXTI9_5_Interrupt;
      pragma Unreferenced (Interrupt_Handler);
   end Com_Port_Reader;

   protected body Com_Port_Reader is
      procedure Interrupt_Handler is
         Com_Port_Pin : Port_Pin;
      begin
         for Port in Com_Ports loop
            Com_Port_Pin := Com_Wires (Port, Rx, Da);
            if Happened (Com_Port_Pin.Pin) then
               Clear_Interrupt (Line => Com_Port_Pin.Pin);
               Set_True        (S => Com_Ports_Semaphore (Port));
            end if;
         end loop;
      end Interrupt_Handler;
   end Com_Port_Reader;

   procedure Com_Port_Check (Com_Port_Id : Com_Ports) is
      Arrived_Cycle_Time : Time_Span;
   begin
      -- Wait for the first signal.
      Suspend_Until_True (S => Com_Ports_Semaphore (Com_Port_Id));
      -- Toggle.
      Toggle ((Com_Port_Id, L));
      -- Calculate the arrived period time.
      Arrived_Cycle_Time := Clock - Current_Cycle_Start;
      -- Update the offset time.
      if Arrived_Cycle_Time < Half_Single_Period then
         -- Only offset the one after current.
         Next_Cycle_Start := Next_Cycle_Should_Start + Arrived_Cycle_Time;
      end if;
   end Com_Port_Check;

   task Com_Port_Checker_One with
     Storage_Size => 4 * 1024,
     Priority     => Default_Priority;

   task body Com_Port_Checker_One is
   begin
      loop
         -- Check the com port.
         Com_Port_Check (1);
      end loop;
   end Com_Port_Checker_One;

   task Com_Port_Checker_Two with
     Storage_Size => 4 * 1024,
     Priority     => Default_Priority;

   task body Com_Port_Checker_Two is
   begin
      loop
         -- Check the com port.
         Com_Port_Check (2);
      end loop;
   end Com_Port_Checker_Two;

   task Signal_Generator with
     Storage_Size => 4 * 1024,
     Priority     => Default_Priority;

   task body Signal_Generator is
      Next_Sample_Start : Time := System_Start;
      Sine_Sample : Float := 0.0;
      Sine_X : Float := 0.0;
   begin
      loop
         -- Update the current cycle start time.
         Current_Cycle_Start := Clock;
         -- Calculate the suppose start time.
         Next_Cycle_Should_Start := Current_Cycle_Start + Sine_Single_Period;
         -- Update the start time.
         Next_Cycle_Start := Next_Cycle_Should_Start;
         -- Reset the sine x.
         Sine_X := 0.0;
         -- Initial the next sample start time.
         Next_Sample_Start := Current_Cycle_Start;
         -- Sample.
         for Detect_Time in Sample_Range loop
            Next_Sample_Start := Next_Sample_Start + Calc_Signal_Period;
            -- Calculate the sample.
            Sine_Sample := Sine (Sine_X);
            Sine_X := Sine_X + Sample_X_Slide;
            -- Update the light, write the signal.
            if Sine_Sample < 0.0 then
               for Port in Com_Ports'First .. Com_Ports'Last loop
                  Reset  (Port);
                  Off ((Port, R));
               end loop;
            else
               for Port in Com_Ports'First .. Com_Ports'Last loop
                  Set  (Port);
                  On ((Port, R));
               end loop;
            end if;
            -- Time limitation for one single sample.
            delay until Next_Sample_Start;
         end loop;
         -- Wait to the suppose time.
         delay until Next_Cycle_Should_Start;
         -- Wait until next sine start
         delay until Next_Cycle_Start;
      end loop;
   end Signal_Generator;
begin
   -- Initialized the com ports with the interruptions.
   declare
      Com_Port_Pin : Port_Pin;
   begin
      for Port in Com_Ports loop
         -- Get the port wire data.
         Com_Port_Pin := Com_Wires (Port, Rx, Da);
         Enable (System_Configuration_Contr);
         Set_Interrupt_Source (Interrupt_No => External_Interrupt_No (Com_Port_Pin.Pin),
                               Port => Com_Port_Pin.Port);
         Set_Trigger (Line    => Com_Port_Pin.Pin,
                      Raising => Enable,
                      Falling => Disable);
         Masking (Line => Com_Port_Pin.Pin,
                  State => Unmasked);
      end loop;
   end;
end Generator_Controllers;
