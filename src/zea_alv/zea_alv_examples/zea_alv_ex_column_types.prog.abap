*&---------------------------------------------------------------------*
*& Report zea_alv_ex_column_types
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zea_alv_ex_column_types.

CLASS lcl_alv DEFINITION INHERITING FROM zcl_ea_alv_table CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      prepare_table.

  PROTECTED SECTION.
    METHODS:
      on_button_click REDEFINITION,
      on_hotspot_click REDEFINITION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_output,
        description       TYPE string,
        button            TYPE string,
        hotspot           TYPE string,
        colored_cell      TYPE string,
        color             TYPE columns->tt_color_col,
        cell_style        TYPE columns->tt_cell_style_col,
        exception         TYPE columns->t_exception_col,
        tt_cell_style_col TYPE lvc_t_styl,
      END OF t_output,
      tt_output TYPE STANDARD TABLE OF t_output WITH EMPTY KEY.

    DATA:
        output TYPE tt_output.
ENDCLASS.

CLASS lcl_alv IMPLEMENTATION.
  METHOD prepare_table.
    "Remember, that all collumn names must be uppercase
    APPEND VALUE #( description = 'Color' color = VALUE #(  ( fname = 'COLORED_CELL' color = VALUE #( col = 6 ) ) ( fname = 'DESCRIPTION' color = VALUE #( col = 5 ) ) ) ) TO output.
    APPEND VALUE #( description = '@WB\QTOOLTIP@Text with icon and tooltip'  ) TO output.
    APPEND VALUE #( description = 'Row with exception' exception = '2'  ) TO output.

    APPEND VALUE #( description = 'Button with icon' button = '@01@Why is this text not displayed?' cell_style = VALUE #( (  fieldname  = 'BUTTON' style = cl_gui_alv_grid=>mc_style_button ) ) ) TO output.
    APPEND VALUE #( description = 'Button without icon' button = 'Button' cell_style = VALUE #( (  fieldname  = 'BUTTON' style = cl_gui_alv_grid=>mc_style_button ) ) ) TO output.
    APPEND VALUE #( description = 'Hotspot' hotspot = 'Hotspot' cell_style = VALUE #( (  fieldname  = 'HOTSPOT' style = cl_gui_alv_grid=>mc_style_hotspot ) ) ) TO output.

    APPEND VALUE #( description = 'Editable cell' cell_style = VALUE #( (  fieldname  = 'DESCRIPTION' style = cl_gui_alv_grid=>mc_style_enabled ) ) ) TO output.
    alv_grid->set_ready_for_input( 1 ).

    "Remember to set_data before changing any columns
    set_data( REF #( output ) ).

    columns->set_as_color( 'COLOR' ).
    columns->set_as_cell_style( 'CELL_STYLE' ).
    columns->set_as_exception( 'EXCEPTION' ).
    columns->set_fixed_text( column = 'DESCRIPTION' text = |Description| ).
    columns->set_optimize( ).
  ENDMETHOD.

  METHOD on_button_click.
    MESSAGE |Button clicked: { es_row_no-row_id }/{ es_col_id-fieldname }; Value="{ output[ es_row_no-row_id ]-button }"| TYPE 'S'.
  ENDMETHOD.

  METHOD on_hotspot_click.
    MESSAGE |Hotspot clicked: { es_row_no-row_id }/{ e_column_id-fieldname }; Value="{ output[ es_row_no-row_id ]-hotspot }"| TYPE 'S'.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(alv) = NEW lcl_alv( ).
  alv->prepare_table( ).
  alv->display_data( ).
