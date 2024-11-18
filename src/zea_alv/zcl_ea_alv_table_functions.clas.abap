"! <p class="shorttext synchronized" lang="en">Add custom functions to <em>zcl_ea_alv_table</em>.
"! Redefine <em>on_added_function</em> or hook-up directly to event <em>user_command</em> of <em>cl_gui_alv_grid</em>.
CLASS zcl_ea_alv_table_functions DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_ea_alv_table.
  PUBLIC SECTION.
    METHODS:
      add_function IMPORTING function TYPE stb_button,
      remove_function IMPORTING function TYPE ui_func,
      remove_all_functions.

  PROTECTED SECTION.
    METHODS:
      alv_grid_changed IMPORTING alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object e_interactive.

    DATA:
      functions TYPE STANDARD TABLE OF stb_button.
ENDCLASS.



CLASS ZCL_EA_ALV_TABLE_FUNCTIONS IMPLEMENTATION.


  METHOD add_function.
    APPEND function TO functions.
  ENDMETHOD.


  METHOD alv_grid_changed.
    SET HANDLER me->on_toolbar FOR alv_grid.
  ENDMETHOD.


  METHOD on_toolbar.
    APPEND LINES OF functions TO e_object->mt_toolbar.
  ENDMETHOD.


  METHOD remove_all_functions.
    CLEAR functions.
  ENDMETHOD.


  METHOD remove_function.
    DELETE functions WHERE function = function.
  ENDMETHOD.
ENDCLASS.
