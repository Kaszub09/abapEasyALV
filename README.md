# abapEasyALV
Wrapper classes for ALV/SALV frameworks for easy reporting. Check examples for usage.

### Why both
While SALV framework is newer and more easier to use (especially since it doesn't require container), it lacks some features like being able to edit cells or drag&drop functionality. So SALV is preferred unless you need them.

### Example usage - simple report with ALV. 
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

### Example usage - simple report with SALV. 
```abap
REPORT zea_salv_ex_simple_report.

TABLES: sflight.

SELECT-OPTIONS:
s_carrid FOR sflight-carrid.
PARAMETERS:
p_layout TYPE disvariant-variant.

INITIALIZATION.
  DATA(salv) = NEW zcl_ea_salv_table( ). "Initialize there so you get f4 for layout.

START-OF-SELECTION.
  SELECT * FROM sflight WHERE carrid IN @s_carrid INTO TABLE @DATA(sflight_tab)
  salv->set_data( REF #( sflight_tab ) ).
  salv->display_data( p_layout ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  p_layout = salv->get_layout_from_f4_selection( ).
```
