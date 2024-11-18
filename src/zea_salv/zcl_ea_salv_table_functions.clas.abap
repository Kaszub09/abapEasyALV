"! <p class="shorttext synchronized">Functions for <em>zcl_ea_salv_table</em></p>
CLASS zcl_ea_salv_table_functions DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_ea_salv_table.
  PUBLIC SECTION.
    TYPES:
      t_function TYPE sy-ucomm,
      BEGIN OF t_functions,
        function    TYPE t_function,
        description TYPE smp_dyntxt,
      END OF t_functions,
      tt_functions TYPE STANDARD TABLE OF t_functions WITH EMPTY KEY.

    METHODS:
      constructor IMPORTING alv_table TYPE REF TO cl_salv_table,
      "! <p class="shorttext synchronized" lang="en">Up to 10 functions.</p>
      add_function IMPORTING function TYPE t_function description TYPE smp_dyntxt,
      remove_function IMPORTING function TYPE t_function,
      remove_all_functions.

    EVENTS:
    "! Intermediary handler so we can translate dynamic functions names to the ones supplied.
     added_function EXPORTING VALUE(function) TYPE salv_de_function OPTIONAL.

  PROTECTED SECTION.
    CONSTANTS:
      c_program_name TYPE  syrepid VALUE 'SAPLZEA_SALV',
      "! Copied from program <em>saplsalv_metadata_status</em>, status <em>salv_table_standard</em>
      "! - with some unnecessary functions removed to make place for dynamic functions.
      c_status_name  TYPE  sypfkey  VALUE 'SALV_TABLE'.
    METHODS:
      on_added_function FOR EVENT added_function OF cl_salv_events_table IMPORTING e_salv_function,
      update_function_group.

    DATA:
      functions TYPE tt_functions,
      alv_table TYPE REF TO cl_salv_table.
ENDCLASS.

CLASS zcl_ea_salv_table_functions IMPLEMENTATION.
  METHOD add_function.
    APPEND VALUE #( function = function description = description ) TO functions.
    update_function_group( ).
  ENDMETHOD.

  METHOD constructor.
    me->alv_table = alv_table.
    DATA(event) = alv_table->get_event( ).
    SET HANDLER me->on_added_function FOR event.
  ENDMETHOD.

  METHOD on_added_function.
    DATA(new_function) = e_salv_function.
    IF e_salv_function(8) = 'DYNAMIC_'.
      SPLIT e_salv_function AT '_' INTO DATA(dummy) DATA(function_number).
      new_function = functions[ function_number ]-function.
    ENDIF.

    RAISE EVENT added_function EXPORTING function = new_function.
  ENDMETHOD.

  METHOD remove_function.
    DELETE functions WHERE function = function.
    update_function_group( ).
  ENDMETHOD.

  METHOD remove_all_functions.
    CLEAR functions.
    update_function_group( ).
  ENDMETHOD.

  METHOD update_function_group.
    CALL FUNCTION 'ZEA_SALV_SET_FUNCTIONS' EXPORTING functions = functions.
    alv_table->set_screen_status( report = c_program_name pfstatus = c_status_name ).
  ENDMETHOD.
ENDCLASS.
