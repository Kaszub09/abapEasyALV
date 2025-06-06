"! <p class="shorttext synchronized">Columns information for <em>zcl_ea_salv_table</em></p>
CLASS zcl_ea_salv_table_columns DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_ea_salv_table.

  PUBLIC SECTION.
    TYPES:
      "! 1-blue; 3-yellow; 5-green; 6-red; 7-orange
      tt_color_col     TYPE lvc_t_scol,
      tt_cell_type_col TYPE salv_t_int4_column,
      t_exception_col  TYPE c LENGTH 1.

    METHODS:
      constructor IMPORTING alv_table TYPE REF TO cl_salv_table,
      get IMPORTING column TYPE lvc_fname RETURNING VALUE(column_obj) TYPE REF TO cl_salv_column_table,
      set_fixed_text IMPORTING column TYPE lvc_fname text TYPE csequence,
      set_decimals IMPORTING column TYPE lvc_fname value TYPE lvc_decmls,
      set_ddic_field IMPORTING column TYPE lvc_fname table TYPE lvc_tname field TYPE lvc_fname,
      set_as_hidden IMPORTING column TYPE lvc_fname is_hidden TYPE abap_bool DEFAULT abap_true,
      set_as_color IMPORTING column TYPE lvc_fname,
      "! <p class="shorttext synchronized" lang="en">Check documentation for <em>cl_salv_columns_list=>set_exception_column</em>. Column will be displayed first</p>
      set_as_exception IMPORTING column TYPE lvc_exfnm group TYPE c DEFAULT '2',
      "! <p class="shorttext synchronized" lang="en">Get types from <em>if_salv_c_celltype</em>.</p>
      set_as_cell_type IMPORTING column TYPE lvc_fname,
      set_as_hotspot IMPORTING column TYPE lvc_fname is_hotspot TYPE abap_bool DEFAULT abap_true,
      set_as_icon IMPORTING column TYPE lvc_fname is_icon TYPE abap_bool DEFAULT abap_true,
      set_as_key IMPORTING column TYPE lvc_fname is_key TYPE abap_bool DEFAULT abap_true,
      set_edit_mask IMPORTING column TYPE lvc_fname mask TYPE lvc_edtmsk OPTIONAL,
      set_output_length IMPORTING column TYPE lvc_fname output_length TYPE lvc_outlen,
      "! <p class="shorttext synchronized" lang="en">Rearranges columns and recalculates col_pos.
      "! Column is moved before if parameter is filled, after otherwise.</p>
      move_column IMPORTING column_to_move TYPE lvc_fname before TYPE lvc_fname OPTIONAL after TYPE lvc_fname OPTIONAL,
      "! <p class="shorttext synchronized" lang="en">Warning! Can slow down display if there is too many rows/columns (like tens of thousands)</p>
      set_optimize IMPORTING is_optimized TYPE abap_bool DEFAULT abap_true,
      "! @parameter color | <p class="shorttext synchronized" lang="en">1-blue; 3-yellow; 5-green; 6-red; 7-orange</p>
      set_color IMPORTING column TYPE lvc_fname color TYPE lvc_s_colo,
      set_as_currency IMPORTING column TYPE lvc_fname amount_column TYPE lvc_fname,
      set_as_quantity IMPORTING column TYPE lvc_fname amount_column TYPE lvc_fname,
      set_as_checkbox IMPORTING column TYPE lvc_fname is_checkbox TYPE abap_bool DEFAULT abap_true.

  PROTECTED SECTION.
    DATA:
     alv_table TYPE REF TO cl_salv_table.
ENDCLASS.

CLASS zcl_ea_salv_table_columns IMPLEMENTATION.
  METHOD constructor.
    me->alv_table = alv_table.
  ENDMETHOD.

  METHOD get.
    column_obj = CAST #( alv_table->get_columns( )->get_column( column ) ).
  ENDMETHOD.

  METHOD move_column.
    DATA(new_position) = COND i( WHEN before IS NOT INITIAL THEN alv_table->get_columns( )->get_column_position( before )
                                 ELSE alv_table->get_columns( )->get_column_position( after ) + 1 ).
    "The way it works is that it deletes column_to_set, then inserts it into index.
    "So e.g. if you have 10 columns and try to insert before 9th column:
    "1. If column_to_move is after 9th column, it will work as expected
    "2. If column_to_move is before 9th column, it will be removed and 9th column temporarily becomes 8th, so it's actually inserted after 9th column
    "   - that's why we need to decrement position
    IF new_position > alv_table->get_columns( )->get_column_position( column_to_move ).
      new_position = new_position - 1.
    ENDIF.
    alv_table->get_columns( )->set_column_position( columnname = column_to_move position = new_position ).
  ENDMETHOD.

  METHOD set_as_cell_type.
    alv_table->get_columns( )->set_cell_type_column( column ).
  ENDMETHOD.

  METHOD set_as_color.
    alv_table->get_columns( )->set_color_column( column ).
  ENDMETHOD.

  METHOD set_as_exception.
    alv_table->get_columns( )->set_exception_column( value = column group = group ).
  ENDMETHOD.

  METHOD set_as_hidden.
    alv_table->get_columns( )->get_column( column )->set_technical( is_hidden ).
  ENDMETHOD.

  METHOD set_as_hotspot.
    get( column )->set_cell_type( value = COND #( WHEN is_hotspot = abap_true THEN if_salv_c_cell_type=>hotspot ELSE if_salv_c_cell_type=>text ) ).
  ENDMETHOD.

  METHOD set_as_icon.
    get( column )->set_icon( is_icon ).
  ENDMETHOD.

  METHOD set_as_key.
    get( column )->set_key( is_key ).
  ENDMETHOD.

  METHOD set_ddic_field.
    alv_table->get_columns( )->get_column( column )->set_ddic_reference( VALUE salv_s_ddic_reference( table = table field = field ) ).
  ENDMETHOD.

  METHOD set_edit_mask.
    alv_table->get_columns( )->get_column( column )->set_edit_mask( mask ).
  ENDMETHOD.

  METHOD set_fixed_text.
    DATA(col) = me->alv_table->get_columns( )->get_column( column ).
    col->set_long_text( CONV #( text ) ).
    col->set_fixed_header_text( 'L' ).
    col->set_medium_text( space ).
    col->set_short_text( space ).
  ENDMETHOD.

  METHOD set_decimals.
    alv_table->get_columns( )->get_column( column )->set_decimals( value = value ).
  ENDMETHOD.

  METHOD set_optimize.
    alv_table->get_columns( )->set_optimize( is_optimized ).
  ENDMETHOD.

  METHOD set_output_length.
    alv_table->get_columns( )->get_column( column )->set_output_length( output_length ).
  ENDMETHOD.

  METHOD set_color.
    get( column )->set_color( color ).
  ENDMETHOD.

  METHOD set_as_currency.
    alv_table->get_columns( )->get_column( amount_column )->set_currency_column( column ).
  ENDMETHOD.

  METHOD set_as_quantity.
    alv_table->get_columns( )->get_column( amount_column )->set_quantity_column( column ).
  ENDMETHOD.

  METHOD set_as_checkbox.
    get( column )->set_cell_type( value = COND #( WHEN is_checkbox = abap_true THEN if_salv_c_cell_type=>checkbox ELSE if_salv_c_cell_type=>text ) ).
  ENDMETHOD.

ENDCLASS.
