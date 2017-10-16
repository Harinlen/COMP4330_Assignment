--
-- Uwe R. Zimmer, Australia 2015
--

with Ada.Real_Time;                       use Ada.Real_Time;
with ANU_Base_Board;                      use ANU_Base_Board;
with ANU_Base_Board.Com_Interface;        use ANU_Base_Board.Com_Interface;
with ANU_Base_Board.LED_Interface;        use ANU_Base_Board.LED_Interface;
with Discovery_Board;                     use Discovery_Board;
with Discovery_Board.Button;              use Discovery_Board.Button;
with Discovery_Board.LED_Interface;       use Discovery_Board.LED_Interface;
with STM32F4;                             use type STM32F4.Bit, STM32F4.Bits_32;
with STM32F4.Random_number_generator.Ops; use STM32F4.Random_number_generator.Ops;
with STM32F4.Reset_and_clock_control.Ops; use STM32F4.Reset_and_clock_control.Ops;
with System;                              use System;

package body Generator_Controllers is

   -- Parameters about the sine wave.
   Signal_Frequency : constant Integer := 1;
   Sample_Frequency : constant Integer := 4;

   -- Constants.
   PI : constant Float := 3.1415;
   PI_D : constant Float := PI * 2.0;
   Sine_Omega : constant Float := PI_D * Float (Signal_Frequency);
   type Sine_Taylor_Level is range 1 .. 5;
   Sine_Taylor_Parameters : constant array (Sine_Taylor_Level) of Float := (-0.1666666667,
                                                                            0.0083333333,
                                                                            -0.0001984127,
                                                                            2.75573192239859E-06,
                                                                            -2.50521083854417E-08);
   -- Sample parameters.
   Sample_X_Slide : constant Float := 1000.0 / Float (Sample_Frequency);
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
      Real_X : constant Float := Sine_Omega * X;
   begin
      return Real_X + Sine_Level (Base_X => Real_X,
                                  X_Square => Real_X * Real_X,
                                  Level => Sine_Taylor_Level'First);
   end Sine;

   LED_Pattern : constant array (Com_Ports) of Discovery_Board.LEDs  := (Orange, Red, Blue, Green);

   task Signal_Generator with
     Storage_Size => 4 * 1024,
     Priority     => Default_Priority;

   task body Signal_Generator is
      Next_Sine_Start : Time := System_Start + Sine_Single_Period;
      Next_Sample_Start : Time := System_Start;
      Sine_Sample : Float := 0.0;
   begin
      loop
         On (Red);
         -- Loop for detect time.
         for Detect_Time in Sample_Range loop
            Next_Sample_Start := Next_Sample_Start + Calc_Signal_Period;
            -- Calculate the sample.
            Sine_Sample := Sine (Sample_X_Slide);
            -- Update the light.
--              if Sine_Sample > 0.0 then
--                 On (Blue);
--              else
--                 Off (Blue);
--              end if;
            -- Time limitation for one single sample.
            delay until Next_Sample_Start;
         end loop;
         -- Time limitation for one single period.
         delay until Next_Sine_Start;
      end loop;
   end Signal_Generator;

end Generator_Controllers;
