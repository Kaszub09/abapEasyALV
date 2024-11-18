# abapEasyALV
Wrapper classes for easy reporting. Check examples for usage.

## Example usage - simple report.
```abap
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

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  p_layout = alv->get_layout_from_f4_selection( ).
```
