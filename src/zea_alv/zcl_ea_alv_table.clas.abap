"! <p class="shorttext synchronized">Easy table display</p>
"! Wrapper for CL_GUI_ALV_GRID which is (hopefully) much more pleasant to use and doesn't require custom container.
"! <br/>TAGS: ALV; table; editable; display;
CLASS zcl_ea_alv_table DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES:
      zif_ea_screen_handler.

    METHODS:
      "! @parameter layout_key |  <p class="shorttext synchronized" lang="en">Created from <em>report_id</em> if skipped.</p>
      "! @parameter report_id | <p class="shorttext synchronized" lang="en">Used to if <em>layout_key</em> wasn't filled.</p>
      constructor IMPORTING layout_key TYPE salv_s_layout_key OPTIONAL report_id TYPE sy-repid DEFAULT sy-cprog PREFERRED PARAMETER report_id,
      "! <p class="shorttext synchronized" lang="en"><strong>Warning!</strong>
      "! <br/>Should probably be called inside PBO - unless it's a container that can be created somewhere else,
      "! e.g. cl_gui_docking_container with report name and dynpro.
      set_container IMPORTING container TYPE REF TO cl_gui_container,
      "! <p class="shorttext synchronized" lang="en">Set before changing any columns, since they are based on table.</p>
      "! @parameter create_table_copy | <p class="shorttext synchronized" lang="en">Set to true if table data is destroyed before display
      "!    (e.g. was declared inside method as local variable, and this method ends before calling display data )</p>
      "! @parameter data_table | <p class="shorttext synchronized" lang="en">Should be ref to standard table</p>
      set_data IMPORTING create_table_copy TYPE abap_bool DEFAULT abap_false data_table TYPE REF TO data,
      "! <p class="shorttext synchronized" lang="en"><strong>Warning!</strong>
      "! <br/>Use <em>set_data</em> before displaying. Displays in fullscreen if no container was set.
      "! <br/>If you used default container to display in fullscreen,
      "! you must call <em>close</em> after displaying and before using another instance with default container.
      "! Disregard if you've used your own containers.</p>
      "! @parameter was_save_clicked | <p class="shorttext synchronized" lang="en">Valid only in fullscreen.
      "!    Any data edited by user will stay that way regardless of what was clicked, so create table copy if needed.</p>
      display_data IMPORTING in_edit_mode            TYPE abap_bool DEFAULT abap_false
                   RETURNING VALUE(was_save_clicked) TYPE abap_bool,
      get_layout_from_f4_selection RETURNING VALUE(layout) TYPE slis_vari,
      set_progress_bar IMPORTING text TYPE csequence DEFAULT '' current_record TYPE i DEFAULT 0 records_count TYPE i DEFAULT 0,
      "! @parameter header | <p class="shorttext synchronized" lang="en">Max 70 characters</p>
      "! @parameter header_size | <p class="shorttext synchronized" lang="en">' ' - large; 'M' - medium; 'X' - small;</p>
      set_header IMPORTING header TYPE csequence header_size TYPE lvc_titsz DEFAULT 'M',
      "! <p class="shorttext synchronized" lang="en">Must be called after displaying data in fullscreen.
      "! <br/>Frees control from <em>cl_gui_alv_grid</em> so that container can be reused,
      "!  as well as removes default container if was used to display data in in fullscreen.</p>
      close,
      refresh.

    DATA:
      alv_grid     TYPE REF TO cl_gui_alv_grid READ-ONLY,
      grid_variant TYPE disvariant,
      grid_layout  TYPE  lvc_s_layo,
      "! <p class="shorttext synchronized" lang="en">Data must be set first</p>
      columns      TYPE REF TO zcl_ea_alv_table_columns READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Use to add functions to toolbar. Redefine <em>on_added_function</em> to catch them.</p>
      functions    TYPE REF TO zcl_ea_alv_table_functions READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Defines if the is a drag and drop behaviour.</p>
      "! Use <em>add</em> to add new behaviour, then redefine on_drag/on_drop/on_drop_complete events.
      "! <ul><li>When adding, use flavour to distinguish between behaviours generated/accepted. </li>
      "! <li>Two abap_bools determine if you can drag from or drag into object.</li>
      "! <li>Effect determines only mouse icon (<em>cl_dragdrop</em> constants).</li></ul>
      drag_drop    TYPE REF TO cl_dragdrop READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Info about layout (columns position, whether can be save etc.)</p>
      BEGIN OF layout,
        name             TYPE slis_vari,
        "! <p class="shorttext synchronized" lang="en">'X' - standard; 'U' - user specific; 'A' - both; ' ' - none;</p>
        can_save         TYPE c LENGTH 1 VALUE 'A',
        can_save_initial TYPE abap_bool VALUE abap_true,
      END OF layout.

  PROTECTED SECTION.
    METHODS:
      initialise_alv IMPORTING container TYPE REF TO cl_gui_container OPTIONAL,
      set_handlers,
      "! <p class="shorttext synchronized" lang="en">Called after alv_table initialisation, redefine to e.g. hook up additional handlers after alv_grid recreation.
      "! Should be also manually called in subclass constructor, since base class definition is called in base constructor.</p>
      alv_initialised,
      recreate_default_container,
      destroy_default_container.

    METHODS:
      on_added_function FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no,
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no,
      on_button_click FOR EVENT button_click OF cl_gui_alv_grid IMPORTING es_row_no es_col_id,
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed e_ucomm e_onf4 e_onf4_after e_onf4_before,
      on_drag FOR EVENT ondrag OF cl_gui_alv_grid IMPORTING e_row e_column e_dragdropobj,
      on_drop FOR EVENT ondrop OF cl_gui_alv_grid IMPORTING e_row e_column e_dragdropobj,
      on_drop_complete  FOR EVENT ondropcomplete  OF cl_gui_alv_grid IMPORTING e_row e_column e_dragdropobj.

    DATA:
      data_table_ref         TYPE REF TO data,
      last_progress_bar_text TYPE string.

    DATA:
      default_container       TYPE REF TO cl_gui_docking_container,
      is_in_default_container TYPE abap_bool VALUE abap_true.

    DATA:
      was_save_clicked TYPE abap_bool VALUE abap_false,
      in_edit_mode     TYPE abap_bool VALUE abap_false.
ENDCLASS.



CLASS zcl_ea_alv_table IMPLEMENTATION.


  METHOD alv_initialised.
  ENDMETHOD.


  METHOD close.
    alv_grid->free( ).
    destroy_default_container( ).
  ENDMETHOD.


  METHOD constructor.
    grid_variant = VALUE #( report = report_id username = sy-uname ).
    IF layout_key IS NOT INITIAL.
      grid_variant = CORRESPONDING #( BASE ( grid_variant ) layout_key ).
    ENDIF.

    drag_drop = NEW #( ).
    functions = NEW #( ).
    initialise_alv( ).
  ENDMETHOD.


  METHOD destroy_default_container.
    IF default_container IS BOUND.
      default_container->free( ).
      CLEAR default_container.
    ENDIF.
  ENDMETHOD.


  METHOD display_data.
    me->was_save_clicked = abap_false.
    me->in_edit_mode = in_edit_mode.

    ASSIGN data_table_ref->* TO FIELD-SYMBOL(<table>).
    DATA(fc) = CONV lvc_t_fcat( columns->fc ).
    drag_drop->get_handle( IMPORTING handle = grid_layout-s_dragdrop-grid_ddid ).

    alv_grid->set_table_for_first_display( EXPORTING is_variant = VALUE #( BASE grid_variant variant = layout-name )
        i_save = layout-can_save i_default = layout-can_save_initial is_layout = grid_layout
                                           CHANGING it_outtab = <table> it_fieldcatalog = fc ).

    IF is_in_default_container = abap_true.
      zcl_ea_screen=>set_event_handler( me ).
      zcl_ea_screen=>display_screen( ).
      zcl_ea_screen=>clear_event_handler( ).

      was_save_clicked = me->was_save_clicked.
    ENDIF.
  ENDMETHOD.


  METHOD get_layout_from_f4_selection.
    layout = cl_salv_layout_service=>f4_layouts( s_key = CORRESPONDING #( grid_variant ) restrict = if_salv_c_layout=>restrict_none )-layout.
  ENDMETHOD.


  METHOD initialise_alv.
    destroy_default_container( ).

    IF container IS BOUND.
      is_in_default_container = abap_false.
      alv_grid = NEW cl_gui_alv_grid( i_parent = container ).
    ELSE.
      is_in_default_container = abap_true.
      IF cl_gui_alv_grid=>offline( ) = 0.
        "^Non-initial container raises error in background
        recreate_default_container( ).
      ENDIF.
      alv_grid = NEW cl_gui_alv_grid( i_parent = default_container ).
    ENDIF.

    IF cl_gui_alv_grid=>offline( ) = 0.
      "^Those events raise errors in background
      alv_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_enter ).
      alv_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).
    ENDIF.

    set_handlers( ).
    functions->alv_grid_changed( alv_grid ).

    alv_initialised( ).
  ENDMETHOD.


  METHOD on_added_function.
  ENDMETHOD.


  METHOD on_button_click.
  ENDMETHOD.


  METHOD on_data_changed.
  ENDMETHOD.


  METHOD on_double_click.
  ENDMETHOD.


  METHOD on_drag.
  ENDMETHOD.


  METHOD on_drop.
  ENDMETHOD.


  METHOD on_drop_complete.
  ENDMETHOD.


  METHOD on_hotspot_click.
  ENDMETHOD.


  METHOD recreate_default_container.
    destroy_default_container( ).
    default_container = NEW cl_gui_docking_container( repid = zcl_ea_screen=>c_program_name dynnr = zcl_ea_screen=>c_screen_without_toolbar
      side = cl_gui_docking_container=>dock_at_top ratio = 95 ).
    default_container->set_extension( 9999 ).
  ENDMETHOD.


  METHOD refresh.
    alv_grid->refresh_table_display( ).
  ENDMETHOD.


  METHOD set_container.
    initialise_alv( container ).
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
    ELSE.
      me->data_table_ref = data_table.
    ENDIF.

    columns = NEW #( data_table = data_table_ref grid_layout = REF #( grid_layout ) ).
  ENDMETHOD.


  METHOD set_handlers.
    SET HANDLER on_added_function on_double_click on_hotspot_click on_button_click on_data_changed on_drag on_drop on_drop_complete FOR alv_grid.
  ENDMETHOD.


  METHOD set_header.
    grid_layout-grid_title = CONV #( header ).
    grid_layout-smalltitle = header_size.
  ENDMETHOD.


  METHOD set_progress_bar.
    IF NOT text IS INITIAL.
      last_progress_bar_text = text.
    ENDIF.

    IF records_count > 0.
      cl_progress_indicator=>progress_indicate( i_text = last_progress_bar_text i_processed = current_record
        i_total = records_count i_output_immediately = abap_true ).
    ELSE.
      cl_progress_indicator=>progress_indicate( i_text = last_progress_bar_text i_output_immediately = abap_true ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_ea_screen_handler~pai.
    CASE command.
      WHEN 'SAVE'.
        was_save_clicked = abap_true.
        LEAVE TO SCREEN 0.

      WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_ea_screen_handler~pbo.
    IF in_edit_mode = abap_true.
      SET PF-STATUS 'ALV_WITH_SAVE' OF PROGRAM zcl_ea_screen=>c_program_name.
    ELSE.
      SET PF-STATUS 'ALV_WITH_SAVE' OF PROGRAM zcl_ea_screen=>c_program_name EXCLUDING 'SAVE'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
