*&---------------------------------------------------------------------*
*& Report zea_alv_screen_with_functions
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zea_alv_screen_popup_with_cont.

CLASS lcl_handler DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_ea_screen_handler.
    DATA:
        data_table TYPE REF TO data.


  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      free_alv.
    DATA:
      container TYPE REF TO cl_gui_docking_container,
      alv       TYPE REF TO zcl_ea_alv_table.
ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.

  METHOD zif_ea_screen_handler~pbo.
    SET PF-STATUS zcl_ea_screen=>c_main_status OF PROGRAM zcl_ea_screen=>c_program_name EXCLUDING 'P+'.
    SET TITLEBAR zcl_ea_screen=>c_main_title OF PROGRAM zcl_ea_screen=>c_program_name WITH 'My title'.

    IF alv IS NOT BOUND.
      alv = NEW #( ).
      container = NEW cl_gui_docking_container(  side = cl_gui_docking_container=>dock_at_top ratio = 95 ).
      alv->set_container( container ).
      alv->set_data( data_table ).
      alv->display_data( ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_ea_screen_handler~pai.
    MESSAGE |Function code { command } called| TYPE 'S'.

    IF command = 'CANCEL'.
      free_alv( ).
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDMETHOD.

  METHOD free_alv.
    container->free( ).
    FREE: alv, container.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  SELECT * FROM sflight INTO TABLE @DATA(sflight_tab).
  SELECT * FROM tadir UP TO 10 ROWS INTO TABLE @DATA(tadir_tab).

  "You can create and use container like that there
  " data(container) = NEW cl_gui_docking_container( repid = zcl_ea_screen=>c_program_name dynnr = zcl_ea_screen=>c_screen_without_toolbar side = cl_gui_docking_container=>dock_at_top ratio = 95 ).
  " container->set_extension( 9999 ).
  "or create it in PBO, especially if you are displaying screen as popup.
  "Note which screen is displayed depends if commands were supplied. Remember to free container after.

  DATA(handler) = NEW lcl_handler( ).
  handler->data_table = REF #( sflight_tab ).
  zcl_ea_screen=>set_event_handler( handler ).
  zcl_ea_screen=>display_screen( commands = VALUE #( ( command  = 'F1' description = VALUE #( icon_id = '@01@' icon_text = 'Function 1' ) ) )
    start_column = 1 start_line = 1 ).
  zcl_ea_screen=>clear_event_handler( ).

  "Call it again just to illustrate, that if container wasn't freed, screen would have multiple containers
  handler = NEW lcl_handler( ).
  handler->data_table = REF #( tadir_tab ).
  zcl_ea_screen=>set_event_handler( handler ).
  zcl_ea_screen=>display_screen( commands = VALUE #( ( command  = 'F2' description = VALUE #( icon_id = '@01@' icon_text = 'Function 2' ) ) )
    start_column = 1 start_line = 1 ).
  zcl_ea_screen=>clear_event_handler( ).
