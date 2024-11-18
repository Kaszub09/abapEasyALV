*&---------------------------------------------------------------------*
*& Report zea_alv_ex_simple_report
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zea_salv_ex_simple_report.

TABLES: sflight.

SELECT-OPTIONS:
s_carrid FOR sflight-carrid.
PARAMETERS:
p_layout TYPE disvariant-variant.

INITIALIZATION.
  DATA(salv) = NEW zcl_ea_salv_table( ). "Initialize there so you get f4 for layout.

START-OF-SELECTION.
  SELECT * FROM sflight WHERE carrid IN @s_carrid INTO TABLE @DATA(sflight_tab).
  "Can be used inside a loop with <em>sy-tabix</em> and <em>lines( )</em> if there is one inside program.
  salv->set_progress_bar( text = |Processing record... { 44 }/{ 100 }| current_record = 44 records_count = 100 ).
  salv->set_data( REF #( sflight_tab ) ).
  salv->display_data( p_layout ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  p_layout = salv->get_layout_from_f4_selection( ).
