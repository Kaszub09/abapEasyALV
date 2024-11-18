*&---------------------------------------------------------------------*
*& Report zea_alv_ex_fullscreen_display
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zea_alv_ex_fullscreen_display.

SELECT * FROM sflight INTO TABLE @DATA(sflight).

DATA(alv) = NEW zcl_ea_alv_table(  ).
alv->set_data( REF #( sflight ) ).
alv->display_data( ).
