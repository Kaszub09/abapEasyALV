"! <p class="shorttext synchronized" lang="en">Class for building field catalogue table for <em>cl_gui_alv_grid</em>.</p>
"! Tips:
"! <ul>
"! <li>For character column, '[at symbol]01\QTooltip[at symbol]Text' will display icon and text next to it, as well as set tooltip for a cell. Check ICON_CREATE function module.</li>
"! <li>If you make single cells editable via styles, remember to call method <em>set_ready_for_input</em> of <em>cl_gui_alv_grid</em></li>
"! </ul>
"! <br/>TAGS: field catalogue; cl_gui_alv_grid
CLASS zcl_ea_alv_table_columns DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      tt_field_cat      TYPE STANDARD TABLE OF lvc_s_fcat WITH EMPTY KEY
          WITH NON-UNIQUE SORTED KEY name COMPONENTS fieldname
          WITH NON-UNIQUE SORTED KEY key_col COMPONENTS key
          WITH NON-UNIQUE SORTED KEY col_pos COMPONENTS col_pos,
      "! 1-blue; 3-yellow; 5-green; 6-red; 7-orange
      tt_color_col      TYPE lvc_t_scol,
      tt_cell_style_col TYPE lvc_t_styl,
      t_exception_col   TYPE c LENGTH 1.

    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Uses <em>cl_salv_data_descr=&gt;read_structdescr</em></p>
      build_fc_from_table IMPORTING data_table TYPE REF TO data RETURNING VALUE(field_catalogue) TYPE tt_field_cat.

    METHODS:
      "! @parameter grid_layout | <p class="shorttext synchronized" lang="en">Supplied by <em>zcl_ea_alv_table</em>, stores info about special columns.</p>
      constructor IMPORTING data_table TYPE REF TO data grid_layout TYPE REF TO lvc_s_layo OPTIONAL,
      set_fixed_text IMPORTING column TYPE lvc_fname text TYPE csequence,
      "! <p class="shorttext synchronized" lang="en">Mark column as field from table that has foreign key, in order to check values against that table when editing.</p>
      set_ddic_field IMPORTING column TYPE lvc_fname table TYPE lvc_tname field TYPE lvc_fname,
      set_as_hidden IMPORTING column TYPE lvc_fname is_hidden TYPE abap_bool DEFAULT abap_true,
      set_as_color IMPORTING column TYPE lvc_ctfnm,
      "! <p class="shorttext synchronized" lang="en">Check documentation for <em>cl_salv_columns_list=>set_exception_column</em>. Column will be displayed first</p>
      "! @parameter data_el_to_display_col_as | <p class="shorttext synchronized" lang="en">Column will get title and F4 from given data_element</p>
      set_as_exception IMPORTING column TYPE lvc_exfnm group TYPE c DEFAULT '2'
                                 data_element TYPE lvc_exrol DEFAULT space display_in_subtotals TYPE abap_bool DEFAULT abap_false,
      "! <p class="shorttext synchronized" lang="en">Get styles from <em>cl_gui_alv_grid</em>, e.g. <em>cl_gui_alv_grid=>mc_style_button</em> for button</p>
      set_as_cell_style IMPORTING column TYPE lvc_exfnm,
      set_as_hotspot IMPORTING column TYPE lvc_fname is_hotspot TYPE abap_bool DEFAULT abap_true,
      set_as_icon IMPORTING column TYPE lvc_fname is_icon TYPE abap_bool DEFAULT abap_true,
      set_as_key IMPORTING column TYPE lvc_fname is_key TYPE abap_bool DEFAULT abap_true,
      set_as_editable IMPORTING column TYPE lvc_fname is_editable TYPE abap_bool DEFAULT abap_true,
      set_edit_mask IMPORTING column TYPE lvc_fname mask TYPE lvc_edtmsk OPTIONAL,
      set_output_length IMPORTING column TYPE lvc_fname output_length TYPE lvc_outlen,
      "! <p class="shorttext synchronized" lang="en">Rearranges columns and recalculates col_pos.
      "! Column is moved before if parameter is filled, after otherwise.</p>
      move_column IMPORTING column_to_move TYPE lvc_fname before TYPE lvc_fname OPTIONAL after TYPE lvc_fname OPTIONAL,
      "! <p class="shorttext synchronized" lang="en">Warning! Can slow down display if there is too many rows/columns (like tens of thousands)</p>
      set_optimize IMPORTING is_optimized TYPE abap_bool DEFAULT abap_true,
      set_all_as_editable IMPORTING is_editable TYPE abap_bool DEFAULT abap_true,
      "! @parameter column | <p class="shorttext synchronized" lang="en">Currency column</p>
      "! @parameter for_column | <p class="shorttext synchronized" lang="en">Amount column</p>
      set_as_currency IMPORTING column TYPE lvc_fname for_column TYPE lvc_fname,
      "! @parameter color | <p class="shorttext synchronized" lang="en">"! 1-blue; 3-yellow; 5-green; 6-red; 7-orange</p>
      set_color IMPORTING column TYPE lvc_fname color TYPE lvc_s_colo,
      set_as_checkbox IMPORTING column TYPE lvc_fname is_checkbox TYPE abap_bool DEFAULT abap_true,
      "! @parameter column | <p class="shorttext synchronized" lang="en">Qunatity column</p>
      "! @parameter for_column | <p class="shorttext synchronized" lang="en">Amount column</p>
      set_as_quantity IMPORTING column TYPE lvc_fname for_column TYPE lvc_fname.

    DATA:
      fc        TYPE tt_field_cat.
  PRIVATE SECTION.
    DATA:
        grid_layout TYPE REF TO lvc_s_layo.
ENDCLASS.



CLASS zcl_ea_alv_table_columns IMPLEMENTATION.


  METHOD build_fc_from_table.
    DATA(table_descr) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data_ref( data_table ) ).
    DATA(line_struct) = CAST cl_abap_structdescr( table_descr->get_table_line_type( ) ).
    DATA(fields) = cl_salv_data_descr=>read_structdescr( line_struct ).

    DATA(index) = 1.
    LOOP AT fields REFERENCE INTO DATA(field).
      DATA(fc_field) = CORRESPONDING lvc_s_fcat( field->* MAPPING key = keyflag ref_field = lfieldname ref_table = tabname dd_roll = rollname ).
      fc_field-col_pos = index.

      IF fc_field-domname = 'XFELD' OR fc_field-domname = 'XFIELD'.
        fc_field-checkbox = abap_true.
      ENDIF.

      IF fc_field-domname IS INITIAL.
        fc_field-coltext = field->fieldtext.
      ENDIF.

      fc_field-tabname = 'TABLE1'.
      APPEND fc_field TO field_catalogue.

      index = index + 1.
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    fc = build_fc_from_table( data_table ).

    me->grid_layout = grid_layout.
    IF me->grid_layout IS NOT BOUND.
      me->grid_layout = NEW #( ).
    ENDIF.
  ENDMETHOD.


  METHOD move_column.
    DATA(column_copy) = fc[ KEY name fieldname = column_to_move ].
    DELETE fc INDEX line_index( fc[ KEY name fieldname = column_to_move ] ) USING KEY name.
    IF before IS NOT INITIAL.
      INSERT column_copy INTO fc INDEX line_index( fc[ fieldname = before ] ). "Skip secondary key to insert into right position
    ELSE.
      INSERT column_copy INTO fc INDEX ( line_index( fc[ fieldname = after ] ) + 1 ). "Skip secondary key to insert into right position
    ENDIF.

    DATA(index) = 1.
    LOOP AT fc REFERENCE INTO DATA(col).
      col->col_pos = index.
      index = index + 1.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_as_cell_style.
    grid_layout->stylefname = column.
  ENDMETHOD.


  METHOD set_as_color.
    grid_layout->ctab_fname = column.
  ENDMETHOD.


  METHOD set_as_editable.
    fc[ KEY name fieldname = column ]-edit = is_editable.
  ENDMETHOD.


  METHOD set_as_exception.
    grid_layout->excp_fname = column.
    grid_layout->excp_group = group.
    grid_layout->excp_rolln = data_element.
    grid_layout->excp_conds = display_in_subtotals.
  ENDMETHOD.


  METHOD set_as_hidden.
    fc[ KEY name fieldname = column ]-tech = is_hidden.
  ENDMETHOD.


  METHOD set_as_hotspot.
    fc[ KEY name fieldname = column ]-hotspot = is_hotspot.
  ENDMETHOD.


  METHOD set_as_icon.
    fc[ KEY name fieldname = column ]-icon = is_icon.
  ENDMETHOD.


  METHOD set_as_key.
    fc[ KEY name fieldname = column ]-key = is_key.
  ENDMETHOD.


  METHOD set_ddic_field.
    DATA(col) = REF #( fc[ KEY name fieldname = column ] ).
    col->ref_table = table.
    col->ref_field = field.
  ENDMETHOD.


  METHOD set_edit_mask.
    fc[ KEY name fieldname = column ]-edit_mask = mask.
  ENDMETHOD.


  METHOD set_fixed_text.
    DATA(col) = REF #( fc[ KEY name fieldname = column ] ).
    col->colddictxt = 'L'.
    CLEAR: col->scrtext_l, col->scrtext_m, col->scrtext_s.
    col->scrtext_l = CONV #( text ).
    col->reptext = CONV #( text ).
    col->coltext = CONV #( text ).
  ENDMETHOD.


  METHOD set_optimize.
    grid_layout->cwidth_opt = is_optimized.
  ENDMETHOD.


  METHOD set_output_length.
    fc[ KEY name fieldname = column ]-outputlen = output_length.
  ENDMETHOD.

  METHOD set_all_as_editable.
    LOOP AT fc REFERENCE INTO DATA(field).
      field->edit = is_editable.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_as_currency.
    fc[ KEY name fieldname = for_column ]-cfieldname = column.
  ENDMETHOD.

  METHOD set_color.
    fc[ KEY name fieldname = column ]-emphasize = |C{ color-col }{ color-int }{ color-inv }|.
  ENDMETHOD.

  METHOD set_as_checkbox.
    fc[ KEY name fieldname = column ]-checkbox = is_checkbox.
  ENDMETHOD.

  METHOD set_as_quantity.
    fc[ KEY name fieldname = for_column ]-qfieldname = column.
  ENDMETHOD.

ENDCLASS.
