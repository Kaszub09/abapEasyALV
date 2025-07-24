*&---------------------------------------------------------------------*
*& Report zea_salv_ex_extension_row_info
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zea_alv_ex_extension_row_info.

TABLES: sflight.

SELECT-OPTIONS:
s_carrid FOR sflight-carrid.

START-OF-SELECTION.
  TYPES:
    BEGIN OF t_output.
      INCLUDE TYPE sflight.
      "Include status / errors / color columns
      INCLUDE TYPE zcl_ea_salv_table_ext_row_info=>t_status_columns.
      TYPES:
    END OF t_output,
    tt_output TYPE STANDARD TABLE OF t_output WITH EMPTY KEY.
  DATA: output TYPE tt_output.

  SELECT FROM sflight
  FIELDS sflight~*,
    "Simulate that data was already processed.
    CASE WHEN substring( carrid, 1, 1 ) = 'A' THEN @abap_true ELSE @abap_false END AS success,
    CASE WHEN substring( carrid, 1, 1 ) = 'A' THEN @space ELSE 'Doesn''t start with A' END AS errors
  WHERE carrid IN @s_carrid
  INTO CORRESPONDING FIELDS OF TABLE @output.

  DATA(salv) = NEW zcl_ea_alv_table_ext_row_info( ).
  salv->set_data( REF #( output ) ).
  "By default marks success rows green, and errors red.
  salv->color_tab_based_on_success( ).
  salv->display_data( ).
