*&---------------------------------------------------------------------*
*& Report zea_alv_ex_add_functions
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zea_salv_ex_add_functions.

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
    MESSAGE |Function "{ function }" called| TYPE 'S'.
  ENDMETHOD.

  METHOD on_double_click.
    MESSAGE |Double click - r={ row }; c={ column }| TYPE 'S'.
  ENDMETHOD.

  METHOD prepare_function.
    functions->add_function( function = 'SUPER_VERY_LONG_FUNCTION WITH SPACES! (not recommended)' description = VALUE #( icon_id = '@00@' icon_text = 'Function' text = 'Function' quickinfo = 'Tooltip' ) ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  SELECT * FROM sflight INTO TABLE @DATA(sflight).
  DATA(salv) = NEW lcl_salv( ).
  salv->prepare_function( ).
  salv->set_data( REF #( sflight ) ).
  salv->display_data( ).
