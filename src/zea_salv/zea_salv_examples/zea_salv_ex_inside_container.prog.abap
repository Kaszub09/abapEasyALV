*&---------------------------------------------------------------------*
*& Report zea_alv_ex_add_functions
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zea_salv_ex_inside_container.

CLASS lcl_salv DEFINITION INHERITING FROM zcl_ea_salv_table.

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
    alv_table->get_functions( )->add_function( name = 'FUNCTION_WITH_LONG_NAME' icon = '@00@' text = 'Function' tooltip = 'TOOLTIP' position = if_salv_c_function_position=>left_of_salv_functions ).
  ENDMETHOD.
ENDCLASS.

PARAMETERS p_dummy TYPE i.
DATA salv TYPE REF TO lcl_salv.

INITIALIZATION.
  SELECT * FROM sflight INTO TABLE @DATA(sflight).

AT SELECTION-SCREEN OUTPUT.
  IF salv IS NOT BOUND.
    salv = NEW lcl_salv( ).
    salv->set_data( REF #( sflight ) ).
    DATA(container) = NEW cl_gui_docking_container( side = cl_gui_docking_container=>dock_at_top ratio = 95 ).
    container->set_extension( 9999 ).
    salv->set_container( container ).
    salv->prepare_function( ).
    salv->display_data( ).
  ENDIF.
