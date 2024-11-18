*&---------------------------------------------------------------------*
*& Report zea_alv_ex_add_functions
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zea_alv_ex_add_functions.

CLASS lcl_alv DEFINITION CREATE PUBLIC INHERITING FROM zcl_ea_alv_table.

  PUBLIC SECTION.
    METHODS:
      prepare_function.
  PROTECTED SECTION.
    METHODS:
      on_double_click REDEFINITION,
      on_added_function REDEFINITION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_alv IMPLEMENTATION.

  METHOD on_added_function.
    IF e_ucomm = 'FUNCTION'.
      MESSAGE |Function "FUNCTION" called| TYPE 'S'.
    ENDIF.
  ENDMETHOD.

  METHOD on_double_click.
    MESSAGE |Double click - r={ e_row-index }; c={ e_column-fieldname }| TYPE 'S'.
  ENDMETHOD.

  METHOD prepare_function.
    functions->add_function( VALUE #( function = 'FUNCTION' icon = '@00@' text = 'Function' quickinfo = 'Tooltip' ) ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  SELECT * FROM sflight INTO TABLE @DATA(sflight).

  DATA(alv) = NEW lcl_alv(  ).
  alv->prepare_function( ).
  alv->set_data( REF #( sflight ) ).
  alv->display_data( ).
