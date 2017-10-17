--
-- Uwe R. Zimmer, Australia 2015
--

with Ada.Interrupts.Names;                        use Ada.Interrupts.Names;
with Ada.Real_Time;                               use Ada.Real_Time;
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
   Signal_Frequency : constant Integer := 1;
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
   Sine_Single_Period_Value : Integer := 1000 / Signal_Frequency;
   Sine_Single_Period : constant Time_Span := Milliseconds (Sine_Single_Period_Value);
   Calc_Signal_Period : constant Time_Span := Milliseconds (Sine_Single_Period_Value / Sample_Frequency);

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

   LED_Pattern : constant array (Com_Ports) of Discovery_Board.LEDs  := (Orange, Red, Blue, Green);

   protected Com_Port_Reader with Interrupt_Priority => Interrupt_Priority'Last is
      procedure Initialized;
   private
      procedure Com_Port_Interrupt_Handler with Attach_Handler => EXTI9_5_Interrupt;
      -- pragma Attach_Handler (Com_Port_Reader.Com_Port_Interrupt_Handler, EXTI9_5_Interrupt);
      pragma Unreferenced (Com_Port_Interrupt_Handler);
   end Com_Port_Reader;

   protected body Com_Port_Reader is
      procedure Initialized is
      begin
         for Port in Com_Ports loop
            -- Get the port wire data.
            declare
               Port_Wire : constant Port_Pin := Com_Wires (Port, Rx, Da);
            begin
               Enable (System_Configuration_Contr);
               Set_Interrupt_Source (Interrupt_No => External_Interrupt_No (Port_Wire.Pin),
                                     Port => Port_Wire.Port);

               Set_Trigger (Line    => Port_Wire.Pin,
                            Raising => Enable,
                            Falling => Disable);
               Masking (Line => Port_Wire.Pin,
                        State => Unmasked);
            end;
         end loop;
      end Initialized;

      procedure Com_Port_Interrupt_Handler is
      begin
         for Port in Com_Ports loop
            declare
               Port_Wire : constant Port_Pin := Com_Wires (Port, Rx, Da);
            begin
               if Happened (Port_Wire.Pin) then
                  Clear_Interrupt (Port_Wire.Pin);
                  On ((Port, L));
               else
                  Off ((Port, L));
               end if;
            end;
         end loop;
      end Com_Port_Interrupt_Handler;
   end Com_Port_Reader;

   task Signal_Generator with
     Storage_Size => 4 * 1024,
     Priority     => Default_Priority;

   task body Signal_Generator is
      Next_Sine_Start : Time := System_Start + Sine_Single_Period;
      Next_Sample_Start : Time := System_Start;
      Sine_Sample : Float := 0.0;
      Sine_X : Float := 0.0;
   begin
      loop
         -- Reset the sine x.
         Sine_X := 0.0;
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
         -- Wait until next sine start
         delay until Next_Sine_Start;
         -- Calculate the new start time.
         Next_Sine_Start := System_Start + Sine_Single_Period;
      end loop;
   end Signal_Generator;

end Generator_Controllers;
