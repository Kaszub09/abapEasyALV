*&---------------------------------------------------------------------*
*& Report zea_alv_ex_add_functions
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zea_salv_ex_select_row.

PARAMETERS p_popup AS CHECKBOX.

START-OF-SELECTION.
  SELECT * FROM sflight INTO TABLE @DATA(sflight).
  DATA(salv) = NEW zcl_ea_salv_table_selectable( ).
  salv->set_data( REF #( sflight ) ).
  IF p_popup = abap_true.
    salv->set_screen_popup( ).
  ENDIF.

  salv->display_selectable( IMPORTING  selected_rows = DATA(selected_rows) user_confirmed = DATA(user_confirmed) ).
  IF user_confirmed = abap_false.
    WRITE |Cancelled|.
  ELSE.
    WRITE |Selected row { selected_rows[ 1 ] } - Date={ sflight[ selected_rows[ 1 ] ]-fldate  }|.
  ENDIF.
