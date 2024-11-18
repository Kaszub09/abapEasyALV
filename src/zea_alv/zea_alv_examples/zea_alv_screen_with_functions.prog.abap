*&---------------------------------------------------------------------*
*& Report zea_alv_screen_with_functions
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zea_alv_screen_with_functions.

CLASS lcl_handler DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES:
      zif_ea_screen_handler.
ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.
  METHOD zif_ea_screen_handler~pbo.
    SET PF-STATUS zcl_ea_screen=>c_main_status OF PROGRAM zcl_ea_screen=>c_program_name EXCLUDING 'P+'.
    SET TITLEBAR zcl_ea_screen=>c_main_title OF PROGRAM zcl_ea_screen=>c_program_name WITH 'My title'.
  ENDMETHOD.

  METHOD zif_ea_screen_handler~pai.
    MESSAGE |Function code { command } called| TYPE 'S'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA(handler) = NEW lcl_handler( ).
  zcl_ea_screen=>set_event_handler( handler ).
  zcl_ea_screen=>display_screen( VALUE #( ( command  = 'F1' description = VALUE #( icon_id = '@01@' icon_text = 'Function 1' ) ) ) ).
  zcl_ea_screen=>clear_event_handler( ).
