pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__generator.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__generator.adb");
pragma Suppress (Overflow_Check);

package body ada_main is

   E005 : Short_Integer; pragma Import (Ada, E005, "ada__real_time_E");
   E079 : Short_Integer; pragma Import (Ada, E079, "system__tasking__protected_objects_E");
   E102 : Short_Integer; pragma Import (Ada, E102, "system__tasking__protected_objects__multiprocessors_E");
   E096 : Short_Integer; pragma Import (Ada, E096, "system__tasking__restricted__stages_E");
   E075 : Short_Integer; pragma Import (Ada, E075, "stm32f4__general_purpose_ios__ops_E");
   E110 : Short_Integer; pragma Import (Ada, E110, "stm32f4__interrupts_and_events__ops_E");
   E113 : Short_Integer; pragma Import (Ada, E113, "stm32f4__system_configuration_controller__ops_E");
   E082 : Short_Integer; pragma Import (Ada, E082, "stm32f4__reset_and_clock_control__ops_E");
   E073 : Short_Integer; pragma Import (Ada, E073, "led_handling_E");
   E086 : Short_Integer; pragma Import (Ada, E086, "discovery_board__led_interface_E");
   E116 : Short_Integer; pragma Import (Ada, E116, "last_chance_handler_E");
   E107 : Short_Integer; pragma Import (Ada, E107, "anu_base_board__com_interface_E");
   E067 : Short_Integer; pragma Import (Ada, E067, "anu_base_board__led_interface_E");
   E089 : Short_Integer; pragma Import (Ada, E089, "generator_controllers_E");


   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");

      procedure Install_Restricted_Handlers_Sequential;
      pragma Import (C,Install_Restricted_Handlers_Sequential, "__gnat_attach_all_handlers");

      Partition_Elaboration_Policy : Character;
      pragma Import (C, Partition_Elaboration_Policy, "__gnat_partition_elaboration_policy");

      procedure Activate_All_Tasks_Sequential;
      pragma Import (C, Activate_All_Tasks_Sequential, "__gnat_activate_all_tasks");

      procedure Start_Slave_CPUs;
      pragma Import (C, Start_Slave_CPUs, "__gnat_start_slave_cpus");
   begin
      Main_Priority := 0;
      Partition_Elaboration_Policy := 'S';

      Ada.Real_Time'Elab_Body;
      E005 := E005 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E079 := E079 + 1;
      System.Tasking.Protected_Objects.Multiprocessors'Elab_Body;
      E102 := E102 + 1;
      System.Tasking.Restricted.Stages'Elab_Body;
      E096 := E096 + 1;
      STM32F4.GENERAL_PURPOSE_IOS.OPS'ELAB_SPEC;
      E075 := E075 + 1;
      E110 := E110 + 1;
      E113 := E113 + 1;
      E082 := E082 + 1;
      E073 := E073 + 1;
      Discovery_Board.Led_Interface'Elab_Body;
      E086 := E086 + 1;
      E116 := E116 + 1;
      Anu_Base_Board.Com_Interface'Elab_Body;
      E107 := E107 + 1;
      Anu_Base_Board.Led_Interface'Elab_Body;
      E067 := E067 + 1;
      Generator_Controllers'Elab_Body;
      E089 := E089 + 1;
      Install_Restricted_Handlers_Sequential;
      Activate_All_Tasks_Sequential;
      Start_Slave_CPUs;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_generator");

   procedure main is
      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      adainit;
      Ada_Main_Program;
   end;

--  BEGIN Object file/option list
   --   C:\Users\u6013763\Documents\Generator\Objects\anu_base_board.o
   --   C:\Users\u6013763\Documents\Generator\Objects\discovery_board.o
   --   C:\Users\u6013763\Documents\Generator\Objects\stm32f4.o
   --   C:\Users\u6013763\Documents\Generator\Objects\stm32f4-general_purpose_ios.o
   --   C:\Users\u6013763\Documents\Generator\Objects\discovery_board-config.o
   --   C:\Users\u6013763\Documents\Generator\Objects\stm32f4-general_purpose_ios-ops.o
   --   C:\Users\u6013763\Documents\Generator\Objects\stm32f4-interrupts_and_events.o
   --   C:\Users\u6013763\Documents\Generator\Objects\stm32f4-interrupts_and_events-ops.o
   --   C:\Users\u6013763\Documents\Generator\Objects\stm32f4-reset_and_clock_control.o
   --   C:\Users\u6013763\Documents\Generator\Objects\stm32f4-system_configuration_controller.o
   --   C:\Users\u6013763\Documents\Generator\Objects\stm32f4-system_configuration_controller-ops.o
   --   C:\Users\u6013763\Documents\Generator\Objects\stm32f4-timers.o
   --   C:\Users\u6013763\Documents\Generator\Objects\stm32f4-reset_and_clock_control-ops.o
   --   C:\Users\u6013763\Documents\Generator\Objects\led_handling.o
   --   C:\Users\u6013763\Documents\Generator\Objects\discovery_board-led_interface.o
   --   C:\Users\u6013763\Documents\Generator\Objects\last_chance_handler.o
   --   C:\Users\u6013763\Documents\Generator\Objects\stm32f4-usart.o
   --   C:\Users\u6013763\Documents\Generator\Objects\anu_base_board-config.o
   --   C:\Users\u6013763\Documents\Generator\Objects\anu_base_board-com_interface.o
   --   C:\Users\u6013763\Documents\Generator\Objects\anu_base_board-led_interface.o
   --   C:\Users\u6013763\Documents\Generator\Objects\generator_controllers.o
   --   C:\Users\u6013763\Documents\Generator\Objects\generator.o
   --   -LC:\Users\u6013763\Documents\Generator\Objects\
   --   -LC:\Users\u6013763\Documents\Generator\Objects\
   --   -LC:\gnat\2017\arm-eabi\lib\gnat\ravenscar-sfp-stm32f4\adalib\
   --   -static
   --   -lgnarl
   --   -lgnat
--  END Object file/option list   

end ada_main;
