"! <p class="shorttext synchronized">Easy table display with SALV framework</p>
"! Wrapper for <em>cl_salv_table</em> which is (hopefully) much more pleasant.
"! If edition or drag&drop events are needed, use <em>zcl_ea_alv_table</em>.
"! <br/>TAGS: ALV; SALV; table; display;
CLASS zcl_ea_salv_table DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      "! @parameter layout_key |  <p class="shorttext synchronized" lang="en">Created from <em>report_id</em> if skipped.</p>
      "! @parameter report_id | <p class="shorttext synchronized" lang="en">Used to if <em>layout_key</em> wasn't filled.</p>
      constructor IMPORTING layout_key TYPE salv_s_layout_key OPTIONAL report_id TYPE sy-repid DEFAULT sy-cprog PREFERRED PARAMETER report_id,
      "! <p class="shorttext synchronized" lang="en"><strong>Warning!</strong>
      "! <br/>Should probably be called inside PBO - unless it's a container that can be created somewhere else,
      "! e.g. cl_gui_docking_container with report name and dynpro.
      "! <br/>It recreates alv_table object, so stuff like data_table, column names, functions etc. must be set again</p>
      set_container IMPORTING container TYPE REF TO cl_gui_container,
      "! <p class="shorttext synchronized" lang="en">Set before changing any columns, since they are based on table.</p>
      "! @parameter create_table_copy | <p class="shorttext synchronized" lang="en">Set to true if table data is destroyed before display
      "!    (e.g. was declared inside method as local variable, and this method ends before calling display data )</p>
      "! @parameter data_table | <p class="shorttext synchronized" lang="en">Should be ref to standard table</p>
      set_data IMPORTING create_table_copy TYPE abap_bool DEFAULT abap_false data_table TYPE REF TO data RAISING cx_salv_no_new_data_allowed,
      "! <p class="shorttext synchronized" lang="en">Set data before displaying with <em>set_data</em>.</p>
      display_data IMPORTING layout_name TYPE slis_vari OPTIONAL,
      get_layout_from_f4_selection RETURNING VALUE(layout) TYPE slis_vari,
      set_progress_bar IMPORTING text TYPE csequence DEFAULT '' current_record TYPE i DEFAULT 0 records_count TYPE i DEFAULT 0,
      "! @parameter header | <p class="shorttext synchronized" lang="en">Max 70 characters</p>
      set_header IMPORTING header TYPE csequence header_size TYPE salv_de_header_size DEFAULT cl_salv_display_settings=>c_header_size_large,
      "! Display screen as popup. Can't be used when displaying in container.
      "! Set start_colum or start_line to 0 to reset to default fullscreen display.
      set_screen_popup IMPORTING start_column TYPE i DEFAULT 1 end_column TYPE i DEFAULT 192
                                 start_line TYPE i DEFAULT 1 end_line TYPE i DEFAULT 32,
      enable_layout_saving IMPORTING restriction TYPE salv_de_layout_restriction DEFAULT if_salv_c_layout=>restrict_none
                                     can_save_as_default TYPE abap_bool DEFAULT abap_true.

    DATA:
      alv_table TYPE REF TO cl_salv_table READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Data must be set first</p>
      columns   TYPE REF TO zcl_ea_salv_table_columns READ-ONLY,
      "! <p class="shorttext synchronized" lang="en"><strong>Warning!</strong>
      "! <br/>Can be used only with default container (so in popup or fullscreen), since it relies on pf-status.
      "! If custom container was supplied, use <em>alv_table->get_functions( )->add_function</em> to add functions to toolbar inside container.</p>
      functions TYPE REF TO zcl_ea_salv_table_functions READ-ONLY.

  PROTECTED SECTION.
    METHODS:
      initialise_alv IMPORTING container TYPE REF TO cl_gui_container OPTIONAL,
      set_handlers,
      "! <p class="shorttext synchronized" lang="en">Called after alv_table initialisation, redefine to e.g. hook up additional handlers after alv_grid recreation.
      "! Should be also manually called in subclass constructor, since base class definition is called in base constructor.</p>
      alv_initialised.

    METHODS:
      on_added_function FOR EVENT added_function OF zcl_ea_salv_table_functions IMPORTING function,
      on_double_click FOR EVENT double_click OF cl_salv_events_table IMPORTING row column,
      on_link_click FOR EVENT link_click OF cl_salv_events_table IMPORTING row column.

    DATA:
      layout_key             TYPE salv_s_layout_key,
      data_table_ref         TYPE REF TO data,
      last_progress_bar_text TYPE string.
ENDCLASS.

CLASS zcl_ea_salv_table IMPLEMENTATION.
  METHOD constructor.
    me->layout_key = COND #( WHEN NOT layout_key IS INITIAL THEN layout_key ELSE VALUE salv_s_layout_key( report = report_id ) ).
    initialise_alv( ).
  ENDMETHOD.

  METHOD display_data.
    IF NOT layout_name IS INITIAL.
      alv_table->get_layout( )->set_initial_layout( layout_name ).
    ENDIF.
    alv_table->display( ).
  ENDMETHOD.

  METHOD enable_layout_saving.
    alv_table->get_layout( )->set_key( layout_key ).
    alv_table->get_layout( )->set_save_restriction( restriction ).
    alv_table->get_layout( )->set_default( can_save_as_default ).
  ENDMETHOD.

  METHOD get_layout_from_f4_selection.
    layout = cl_salv_layout_service=>f4_layouts( s_key = layout_key restrict = if_salv_c_layout=>restrict_none  )-layout.
  ENDMETHOD.

  METHOD initialise_alv.
    IF NOT data_table_ref IS BOUND.
      "Need empty table for cl_salv_table factory, must be of structured type, throws error otherwise
      TYPES: BEGIN OF t_dummy, dummy TYPE i, END OF t_dummy.
      CREATE DATA data_table_ref TYPE TABLE OF t_dummy.
    ENDIF.

    FIELD-SYMBOLS: <data_table> TYPE STANDARD TABLE.
    ASSIGN data_table_ref->* TO <data_table>.
    IF container IS BOUND.
      cl_salv_table=>factory( EXPORTING r_container = container IMPORTING r_salv_table = alv_table CHANGING t_table = <data_table> ).
    ELSE.
      cl_salv_table=>factory( IMPORTING r_salv_table = alv_table CHANGING t_table = <data_table> ).
    ENDIF.

    enable_layout_saving( ).
    set_handlers( ).

    alv_table->get_functions( )->set_all( ).

    columns = NEW #( alv_table ).
    functions = NEW #( alv_table ).
    SET HANDLER me->on_added_function FOR functions.

    alv_initialised( ).
  ENDMETHOD.

  METHOD on_added_function.
  ENDMETHOD.

  METHOD on_double_click.
  ENDMETHOD.

  METHOD on_link_click.
  ENDMETHOD.

  METHOD set_data.
    FIELD-SYMBOLS <data_table> TYPE STANDARD TABLE.

    ASSIGN data_table->* TO <data_table>.

    IF create_table_copy = abap_true.
      "Copy data, it's needed if data_table memory is freed before display_data is called
      CREATE DATA data_table_ref LIKE <data_table>.
      FIELD-SYMBOLS <data_table_local_copy> LIKE <data_table>.
      ASSIGN data_table_ref->* TO <data_table_local_copy>.
      APPEND LINES OF <data_table> TO <data_table_local_copy>.

      alv_table->set_data( CHANGING t_table =  <data_table_local_copy> ).
    ELSE.
      me->data_table_ref = data_table.
      alv_table->set_data( CHANGING t_table = <data_table> ).
    ENDIF.
  ENDMETHOD.

  METHOD set_handlers.
    SET HANDLER me->on_double_click me->on_link_click FOR alv_table->get_event( ).
  ENDMETHOD.

  METHOD set_progress_bar.
    IF NOT text IS INITIAL.
      last_progress_bar_text = text.
    ENDIF.

    IF records_count > 0.
      cl_progress_indicator=>progress_indicate( i_text = last_progress_bar_text i_processed = current_record i_total = records_count i_output_immediately = 'X' ).
    ELSE.
      cl_progress_indicator=>progress_indicate( i_text = last_progress_bar_text i_output_immediately = 'X' ).
    ENDIF.
  ENDMETHOD.

  METHOD set_header.
    me->alv_table->get_display_settings( )->set_list_header( CONV #( header ) ). "TODO check if no overflow
    me->alv_table->get_display_settings( )->set_list_header_size( header_size ).
  ENDMETHOD.

  METHOD set_container.
    initialise_alv( container ).
  ENDMETHOD.

  METHOD alv_initialised.
  ENDMETHOD.

  METHOD set_screen_popup.
    alv_table->set_screen_popup( start_column = start_column end_column = end_column start_line = start_line end_line = end_line ).
  ENDMETHOD.
ENDCLASS.
