CLASS zcl_ea_salv_table_selectable DEFINITION PUBLIC CREATE PUBLIC INHERITING FROM zcl_ea_salv_table.

  PUBLIC SECTION.
    CONSTANTS:
      "! Are added whenever <em>cl_gui_salv_table</em> is recreated. Used to determine user action and close screen.
      BEGIN OF c_functions,
        confirm TYPE zcl_ea_salv_table_functions=>t_function  VALUE 'CONFIRM_SELECTION',
        cancel  TYPE zcl_ea_salv_table_functions=>t_function  VALUE 'CANCEL_SELECTION',
      END OF c_functions.

    METHODS:
      "! @parameter layout_key | <p class="shorttext synchronized" lang="en">Created from <em>report_id</em> if skipped.</p>
      "! @parameter report_id | <p class="shorttext synchronized" lang="en">Used to if <em>layout_key</em> wasn't filled.</p>
      constructor IMPORTING layout_key TYPE salv_s_layout_key OPTIONAL report_id TYPE sy-repid DEFAULT sy-repid,
      "! <p class="shorttext synchronized" lang="en">Cannot be used with container,
      "!    since it requires additional functions, which require additional status, which crashes salv_table.
      "!    Additionally display becomes non-blocking call, and then this function doesn't make much sense anymore.</p>
      "! @parameter double_click_select | <p class="shorttext synchronized" lang="en">Double click selects the row and closes screen.</p>
      "! @parameter user_confirmed | <p class="shorttext synchronized" lang="en">Whether user cancelled or clicked confirm
      "!    (also true selected by double clicking if appllicable).</p>
      display_selectable IMPORTING layout TYPE slis_vari OPTIONAL double_click_select TYPE abap_bool DEFAULT abap_true
                         EXPORTING selected_rows TYPE salv_t_row user_confirmed TYPE abap_bool,
      set_selection_mode IMPORTING mode TYPE i DEFAULT if_salv_c_selection_mode=>single.

  PROTECTED SECTION.

    DATA:
      user_confirmed      TYPE abap_bool,
      double_click_select TYPE abap_bool.

    METHODS:
      on_added_function_selectable FOR EVENT added_function OF zcl_ea_salv_table_functions IMPORTING function,
      on_double_click_selectable FOR EVENT double_click OF cl_salv_events_table IMPORTING row column,
      alv_initialised REDEFINITION.
ENDCLASS.

CLASS zcl_ea_salv_table_selectable IMPLEMENTATION.
  METHOD constructor.
    super->constructor( layout_key = layout_key report_id = report_id ).
    "Must be explicitly called since base constructor calls base implementation.
    alv_initialised( ).
  ENDMETHOD.

  METHOD display_selectable.
    me->user_confirmed = abap_false.
    me->double_click_select = double_click_select.

    IF NOT layout IS INITIAL.
      alv_table->get_layout( )->set_initial_layout( layout ).
    ENDIF.

    alv_table->display( ).

    selected_rows = alv_table->get_selections( )->get_selected_rows( ).
    user_confirmed = me->user_confirmed.
  ENDMETHOD.

  METHOD on_added_function_selectable.
    CASE function.
      WHEN c_functions-confirm.
        user_confirmed = abap_true.
        alv_table->close_screen( ).
      WHEN c_functions-cancel.
        user_confirmed = abap_false.
        alv_table->close_screen( ).
    ENDCASE.
  ENDMETHOD.

  METHOD on_double_click_selectable.
    IF double_click_select = abap_false.
      RETURN.
    ENDIF.

    alv_table->get_selections( )->set_selected_rows( value = VALUE #( ( row ) ) ).
    user_confirmed = abap_true.
    alv_table->close_screen( ).
  ENDMETHOD.

  METHOD set_selection_mode.
    alv_table->get_selections( )->set_selection_mode( mode ).
  ENDMETHOD.

  METHOD alv_initialised.
    functions->add_function( function = c_functions-confirm description = VALUE #( icon_id = icon_okay text = TEXT-001 icon_text = TEXT-001 ) ).
    functions->add_function( function = c_functions-cancel description = VALUE #( icon_id = icon_cancel text = TEXT-002 icon_text = TEXT-002 ) ).
    SET HANDLER on_added_function_selectable FOR me->functions.
    SET HANDLER on_double_click_selectable FOR alv_table->get_event( ).
    set_selection_mode( ).
  ENDMETHOD.
ENDCLASS.
