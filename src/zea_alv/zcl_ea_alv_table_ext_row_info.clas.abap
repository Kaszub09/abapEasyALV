"! <p class="shorttext synchronized">Extension to support adding status columns / coloring.
"! <br/>E.g. you are processing rows and want to mark then as successful (or not) and color them.</p>
"! <br/>TAGS: ALV; SALV; table; display; popup; progress bar; extended; color;
CLASS zcl_ea_alv_table_ext_row_info DEFINITION PUBLIC INHERITING FROM zcl_ea_alv_table CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES:
      "!You can include this type in output structure
      BEGIN OF t_status_columns,
        success TYPE ztea_success,
        errors  TYPE ztea_errors,
        color   TYPE lvc_t_scol,
      END OF t_status_columns.

    CONSTANTS:
      BEGIN OF c_color,
        BEGIN OF default,
          col TYPE  lvc_s_colo-col VALUE 0,
        END OF default,
        BEGIN OF blue_heading,
          col TYPE  lvc_s_colo-col VALUE 1,
        END OF blue_heading,
        BEGIN OF blue_default,
          col TYPE  lvc_s_colo-col VALUE 2,
        END OF blue_default,
        BEGIN OF yellow,
          col TYPE  lvc_s_colo-col VALUE 3,
        END OF yellow,
        BEGIN OF blue_key,
          col TYPE  lvc_s_colo-col VALUE 4,
        END OF blue_key,
        BEGIN OF green,
          col TYPE  lvc_s_colo-col VALUE 5,
        END OF green,
        BEGIN OF red,
          col TYPE  lvc_s_colo-col VALUE 6,
        END OF red,
        BEGIN OF orange,
          col TYPE  lvc_s_colo-col VALUE 7,
        END OF orange,
      END OF c_color,
      BEGIN OF c_color_int,
        BEGIN OF default,
          col TYPE  lvc_s_colo-col VALUE 0,
          int TYPE  lvc_s_colo-int VALUE 1,
        END OF default,
        BEGIN OF blue_heading,
          col TYPE  lvc_s_colo-col VALUE 1,
          int TYPE  lvc_s_colo-int VALUE 1,
        END OF blue_heading,
        BEGIN OF blue_default,
          col TYPE  lvc_s_colo-col VALUE 2,
          int TYPE  lvc_s_colo-int VALUE 1,
        END OF blue_default,
        BEGIN OF yellow,
          col TYPE  lvc_s_colo-col VALUE 3,
          int TYPE  lvc_s_colo-int VALUE 1,
        END OF yellow,
        BEGIN OF blue_key,
          col TYPE  lvc_s_colo-col VALUE 4,
          int TYPE  lvc_s_colo-int VALUE 1,
        END OF blue_key,
        BEGIN OF green,
          col TYPE  lvc_s_colo-col VALUE 5,
          int TYPE  lvc_s_colo-int VALUE 1,
        END OF green,
        BEGIN OF red,
          col TYPE  lvc_s_colo-col VALUE 6,
          int TYPE  lvc_s_colo-int VALUE 1,
        END OF red,
        BEGIN OF orange,
          col TYPE  lvc_s_colo-col VALUE 7,
          int TYPE  lvc_s_colo-int VALUE 1,
        END OF orange,
      END OF c_color_int.

    METHODS:
      set_data REDEFINITION,

      "! <p class="shorttext synchronized" lang="en">Works only if success and color columns are present. Overwrites color field.</p>
      "! @parameter whole_row | <p class="shorttext synchronized" lang="en">If false, colors only success column</p>
      color_tab_based_on_success IMPORTING whole_row TYPE abap_bool DEFAULT abap_true,
      "! <p class="shorttext synchronized" lang="en">Works only if success and color columns are present. Overwrites color field.</p>
      "! @parameter whole_row | <p class="shorttext synchronized" lang="en">If false, colors only success column</p>
      color_row_based_on_success IMPORTING whole_row TYPE abap_bool CHANGING row TYPE any DEFAULT abap_true.

  PROTECTED SECTION.
    CONSTANTS:
      BEGIN OF c_col,
        success TYPE fieldname VALUE 'SUCCESS',
        errors  TYPE fieldname VALUE 'ERRORS',
        color   TYPE fieldname VALUE 'COLOR',
      END OF c_col.

    DATA:
      BEGIN OF  is_col_present,
        color   TYPE abap_bool VALUE abap_false,
        success TYPE abap_bool VALUE abap_false,
      END OF is_col_present.
ENDCLASS.

CLASS zcl_ea_alv_table_ext_row_info IMPLEMENTATION.
  METHOD set_data.
    super->set_data( create_table_copy = create_table_copy data_table = data_table ).

    is_col_present-color = xsdbool( line_exists( me->columns->fc[ KEY name fieldname = c_col-color ] ) ).
    is_col_present-success = xsdbool( line_exists( me->columns->fc[ KEY name fieldname = c_col-success ] ) ).

    IF is_col_present-color = abap_true.
      columns->set_as_color( c_col-color ).
    ENDIF.
  ENDMETHOD.

  METHOD color_tab_based_on_success.
    IF is_col_present-success = abap_false OR is_col_present-color = abap_false.
      RETURN.
    ENDIF.

    FIELD-SYMBOLS: <table> TYPE ANY TABLE.
    ASSIGN me->data_table_ref->* TO <table>.

    LOOP AT <table> ASSIGNING FIELD-SYMBOL(<row>).
      color_row_based_on_success( EXPORTING whole_row = whole_row CHANGING row = <row> ).
    ENDLOOP.
  ENDMETHOD.

  METHOD color_row_based_on_success.
    IF is_col_present-success = abap_false OR is_col_present-color = abap_false.
      RETURN.
    ENDIF.

    FIELD-SYMBOLS: <color> TYPE lvc_t_scol.
    ASSIGN COMPONENT c_col-color OF STRUCTURE row TO <color>.
    ASSIGN COMPONENT c_col-success OF STRUCTURE row TO FIELD-SYMBOL(<success>).

    <color> = VALUE #( ( fname = COND #( WHEN whole_row = abap_true THEN space ELSE c_col-success )
       color = COND #( WHEN <success> = abap_true THEN c_color-green ELSE c_color-red ) ) ).
  ENDMETHOD.
ENDCLASS.
