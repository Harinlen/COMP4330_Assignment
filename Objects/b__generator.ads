pragma Warnings (Off);
pragma Ada_95;
pragma Restrictions (No_Exception_Propagation);
with System;
package ada_main is


   GNAT_Version : constant String :=
                    "GNAT Version: GPL 2017 (20170515-63)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_generator" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure main;
   pragma Export (C, main, "main");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  interfaces%s
   --  system%s
   --  ada.exceptions%s
   --  ada.exceptions%b
   --  interfaces.stm32%s
   --  interfaces.stm32.pwr%s
   --  system.bb%s
   --  system.bb.board_parameters%s
   --  system.bb.mcu_parameters%s
   --  system.bb.mcu_parameters%b
   --  system.machine_code%s
   --  system.parameters%s
   --  system.parameters%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.task_info%s
   --  system.task_info%b
   --  system.unsigned_types%s
   --  interfaces.stm32.rcc%s
   --  system.fat_flt%s
   --  system.stm32%s
   --  system.bb.parameters%s
   --  system.stm32%b
   --  system.assertions%s
   --  system.assertions%b
   --  system.secondary_stack%s
   --  system.secondary_stack%b
   --  ada.tags%s
   --  ada.tags%b
   --  system.multiprocessors%s
   --  system.bb.time%s
   --  system.bb.timing_events%s
   --  system.bb.interrupts%s
   --  system.bb.protection%s
   --  system.bb.cpu_primitives%s
   --  system.bb.board_support%s
   --  system.multiprocessors%b
   --  system.multiprocessors.spin_locks%s
   --  system.multiprocessors.spin_locks%b
   --  system.bb.board_support%b
   --  system.bb.cpu_primitives.multiprocessors%s
   --  system.multiprocessors.fair_locks%s
   --  system.bb.threads%s
   --  system.bb.threads.queues%s
   --  system.bb.threads.queues%b
   --  system.bb.protection%b
   --  system.bb.threads%b
   --  system.bb.cpu_primitives%b
   --  system.bb.cpu_primitives.multiprocessors%b
   --  system.os_interface%s
   --  system.multiprocessors.fair_locks%b
   --  system.bb.time%b
   --  system.bb.timing_events%b
   --  system.bb.interrupts%b
   --  system.task_primitives%s
   --  system.tasking%s
   --  system.tasking.debug%s
   --  system.tasking.debug%b
   --  system.task_primitives.operations%s
   --  system.tasking%b
   --  system.task_primitives.operations%b
   --  ada.real_time%s
   --  ada.real_time%b
   --  ada.real_time.delays%s
   --  ada.real_time.delays%b
   --  system.tasking.protected_objects%s
   --  system.tasking.protected_objects%b
   --  system.tasking.protected_objects.multiprocessors%s
   --  system.tasking.protected_objects.multiprocessors%b
   --  system.tasking.protected_objects.single_entry%s
   --  system.tasking.protected_objects.single_entry%b
   --  system.tasking.restricted%s
   --  system.tasking.restricted.stages%s
   --  system.tasking.restricted.stages%b
   --  ada.task_identification%s
   --  ada.task_identification%b
   --  ada.synchronous_task_control%s
   --  ada.synchronous_task_control%b
   --  system.interrupts%s
   --  system.interrupts%b
   --  ada.interrupts%s
   --  ada.interrupts%b
   --  ada.interrupts.names%s
   --  anu_base_board%s
   --  discovery_board%s
   --  stm32f4%s
   --  stm32f4.general_purpose_ios%s
   --  discovery_board.config%s
   --  stm32f4.general_purpose_ios.ops%s
   --  stm32f4.general_purpose_ios.ops%b
   --  stm32f4.interrupts_and_events%s
   --  stm32f4.interrupts_and_events.ops%s
   --  stm32f4.interrupts_and_events.ops%b
   --  stm32f4.random_number_generator%s
   --  stm32f4.random_number_generator.ops%s
   --  stm32f4.random_number_generator.ops%b
   --  stm32f4.reset_and_clock_control%s
   --  stm32f4.system_configuration_controller%s
   --  stm32f4.system_configuration_controller.ops%s
   --  stm32f4.system_configuration_controller.ops%b
   --  stm32f4.timers%s
   --  stm32f4.reset_and_clock_control.ops%s
   --  stm32f4.reset_and_clock_control.ops%b
   --  discovery_board.button%s
   --  discovery_board.button%b
   --  led_handling%s
   --  led_handling%b
   --  discovery_board.led_interface%s
   --  discovery_board.led_interface%b
   --  last_chance_handler%s
   --  last_chance_handler%b
   --  stm32f4.usart%s
   --  anu_base_board.config%s
   --  anu_base_board.com_interface%s
   --  anu_base_board.com_interface%b
   --  anu_base_board.led_interface%s
   --  anu_base_board.led_interface%b
   --  generator_controllers%s
   --  generator_controllers%b
   --  generator%b
   --  END ELABORATION ORDER

end ada_main;
