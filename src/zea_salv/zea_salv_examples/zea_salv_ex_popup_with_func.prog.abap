*&---------------------------------------------------------------------*
*& Report zea_alv_ex_add_functions
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zea_salv_ex_popup_with_func.

CLASS lcl_salv DEFINITION INHERITING FROM zcl_ea_salv_table CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      prepare_function.

  PROTECTED SECTION.
    METHODS:
      on_double_click REDEFINITION,
      on_added_function REDEFINITION.
ENDCLASS.

CLASS lcl_salv IMPLEMENTATION.
  METHOD on_added_function.
    IF function = 'CLOSE'.
      alv_table->close_screen( ). "Close screen and exit popup. Also closed when user closes popup by X in right top corner.
    ENDIF.
  ENDMETHOD.

  METHOD on_double_click.
    MESSAGE |Double click - r={ row }; c={ column }| TYPE 'S'.
  ENDMETHOD.

  METHOD prepare_function.
    functions->add_function( function = 'CLOSE' description = VALUE #( icon_id = icon_close icon_text = 'Close' text = 'Close' quickinfo = 'Tooltip' ) ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  SELECT * FROM sflight INTO TABLE @DATA(sflight).
  DATA(salv) = NEW lcl_salv( ).
  salv->prepare_function( ).
  salv->set_data( REF #( sflight ) ).
  salv->set_screen_popup( ).
  salv->display_data( ).
