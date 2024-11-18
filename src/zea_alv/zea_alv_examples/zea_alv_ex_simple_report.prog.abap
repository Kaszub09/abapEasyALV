*&---------------------------------------------------------------------*
*& Report zea_alv_ex_simple_report
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zea_alv_ex_simple_report.

TABLES: sflight, tadir.

SELECT-OPTIONS:
s_carrid FOR sflight-carrid,
s_objnam FOR tadir-obj_name DEFAULT 'Z*' OPTION CP SIGN I .
PARAMETERS:
p_layout TYPE disvariant-variant.

INITIALIZATION.
  DATA(alv) = NEW zcl_ea_alv_table( ). "Initialize there so you get f4 for layout.

START-OF-SELECTION.
  SELECT * FROM sflight WHERE carrid IN @s_carrid INTO TABLE @DATA(sflight_tab).
  alv->set_data( REF #( sflight_tab ) ).
  alv->layout-name = p_layout.
  alv->display_data( ).
  "We must close after displaying so that default container is destroyed and can be created for another instance,
  "since screen is shared
  alv->close( ).


  SELECT * FROM tadir WHERE obj_name IN @s_objnam INTO TABLE @DATA(tadir_tab) UP TO 1000 ROWS .
  DATA(alv2) = NEW zcl_ea_alv_table(  ).
  alv2->set_data( REF #( tadir_tab ) ).
  alv2->display_data( ).
  "Don't need to destroy now, it's all destroyed when we go back to selection screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  p_layout = alv->get_layout_from_f4_selection( ).
