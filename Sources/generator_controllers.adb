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

   System_Start : constant Time := Clock;
   Signal_Switch_Period : constant Time_Span := Milliseconds (500); -- Should be 10 to be 50Hz ()
   Detect_Signal_Period : constant Time_Span := Milliseconds (2); -- Should be 5 for be 50Hz (200 times per second)
   Sample_Size : constant Integer := 500;

   LED_Pattern : constant array (Com_Ports) of Discovery_Board.LEDs  := (Orange, Red, Blue, Green);

   type Square_Value is mod 2;

   type Sample_Range is mod Sample_Size;
   type Com_Sample_Data is array (Sample_Range) of Square_Value;

   Bit_One_In_Sample : constant Sample_Range := 250;
   Signal : Square_Value := Square_Value'First;
   Signal_Switch_Time : Time := System_Start;

   Up_Start : Boolean := False;

   task Signal_Generator with
     Storage_Size => 4 * 1024,
     Priority     => Default_Priority;

   task body Signal_Generator is
   begin
      loop
         -- Delay to the specific time.
         Signal_Switch_Time := Signal_Switch_Time + Signal_Switch_Period;
         -- Inverse the value to generate the signal.
         Signal := Square_Value'Succ (Signal);
         -- Display the signal.
         if Signal = 1 then
            -- Set the up start.
            Up_Start := True;
            -- Com_Ports loop for sending all the data
            for Port in Com_Ports'First .. Com_Ports'Last loop
               Set  (Port);
               On ((Port, R));
            end loop;
         else
            -- Com_Ports loop for sending all the data
            for Port in Com_Ports'First .. Com_Ports'Last loop
               Reset  (Port);
               Off ((Port, R));
            end loop;
         end if;
         -- Delay to the specific signal.
         delay until Signal_Switch_Time;
      end loop;
   end Signal_Generator;

   task Com_Sampler with
     Storage_Size => 4 * 1024,
     Priority => Default_Priority;

   -- Sync with the one which is at the last position.
   task body Com_Sampler is
      Next_Sample_Time : Time := System_Start;
      Com_Samples : array (Com_Ports) of Com_Sample_Data;
      Sampling_Pos : Sample_Range := Sample_Range'First;
   begin
      loop
         -- Delay to the specific time.
         Next_Sample_Time := Next_Sample_Time + Detect_Signal_Period;
         -- Check the signal is going up.
         if Up_Start then
            -- Sample all the com ports.
            for Port in Com_Ports'First .. Com_Ports'Last loop
               -- Check the Com cache is the same as current release time or not.
               if Read (Port) = 0 then
                  Com_Samples (Port)(Sampling_Pos) := 0;
                  Off ((Port, L));
               else
                  Com_Samples (Port)(Sampling_Pos) := 1;
                  On ((Port, L));
               end if;
            end loop;
            -- Move to next sampling pos.
            Sampling_Pos := Sampling_Pos + 1;
            -- Check the sampling pos is back to beginning.
            if Sampling_Pos = Sample_Range'First then
               -- Compare the sampling data.
               declare
                  Back_Most_Com_Offset : Sample_Range := 0;
               begin
                  -- Check the data is valid or not.
                  for Port in Com_Ports'First .. Com_Ports'Last loop
                     -- Count the number of 1 in the port sample.
                     declare
                        One_Counter : Sample_Range := 0;
                        Current_Offset : Sample_Range := 0;
                     begin
                        -- Count in the current com port.
                        for Sample_Index in Sample_Range loop
                           if Com_Samples (Port)(Sample_Index) = 1 then
                              One_Counter := One_Counter + 1;
                           end if;
                        end loop;
                        -- Update the light.
                        if One_Counter = Bit_One_In_Sample then

                           On (LED_Pattern (Port));
                           -- If the data is valid, calculate the shift.
                           if Com_Samples (Port)(Sample_Range'First) = 1 then
                              -- Start with 1.
                              -- Find the first sample is 0.
                              for Sample_Index in Sample_Range loop
                                 -- Find the 0.
                                 if Com_Samples (Port)(Sample_Index) = 0 then
                                    -- The offset should be index - (half sample size) - 1.
                                    Current_Offset := Sample_Index - Bit_One_In_Sample - 1;
                                    -- Check the offset is more than half or not, because it is loop.
                                    if Current_Offset < Bit_One_In_Sample then
                                       -- Backward, things we want.
                                       if Current_Offset > Back_Most_Com_Offset then
                                          -- Update the offset.
                                          Back_Most_Com_Offset := Current_Offset;
                                       end if;
                                    end if;
                                    -- Loop complete.
                                    exit;
                                 end if;
                              end loop;
                           else
                              -- Start with 0.
                              -- Find the first 1, and its index is the offset.
                              for Sample_Index in Sample_Range loop
                                 -- Find the 0.
                                 if Com_Samples (Port)(Sample_Index) = 0 then
                                    -- The index itself is the offset.
                                    -- Must be backward.
                                    if Sample_Index > Back_Most_Com_Offset then
                                       -- Update the offset.
                                       Back_Most_Com_Offset := Sample_Index;
                                    end if;
                                    -- Loop complete.
                                    exit;
                                 end if;
                              end loop;
                           end if;
                        else
                           Off (LED_Pattern (Port));
                        end if;
                     end;
                  end loop;
                  -- Update the switch time.
                  for Incrase_Time in Sample_Range loop
                     if Incrase_Time = Back_Most_Com_Offset then
                        exit;
                     end if;
                     Signal_Switch_Time := Signal_Switch_Time + Detect_Signal_Period;
                  end loop;
               end;
            end if;
         end if;
         -- Delay to the specific time.
         delay until Next_Sample_Time;
      end loop;
   end Com_Sampler;

end Generator_Controllers;
