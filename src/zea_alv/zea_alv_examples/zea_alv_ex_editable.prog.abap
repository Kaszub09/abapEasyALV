*&---------------------------------------------------------------------*
*& Report zea_alv_ex_editable
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zea_alv_ex_editable.

SELECT 'AA' AS carrid_copy, carrid, connid, fldate FROM sflight INTO TABLE @DATA(sflight_ext).

DATA(alv) = NEW zcl_ea_alv_table(  ).
alv->set_data( REF #( sflight_ext ) ).
alv->columns->set_as_editable( 'CARRID' ).
alv->columns->set_as_editable( 'CARRID_COPY' ).
"Mark column as field from table that has foreign key, in order to check values against that table.
"So column CARRID_COPY is marked as field SFLIGHT-CARRID, which is checked against SCARR.
"Useful if you need to edit but values aren't checked.
alv->columns->set_ddic_field( column = 'CARRID_COPY' table = 'SFLIGHT' field = 'CARRID' ).
DATA(edit_result) = alv->display_data( in_edit_mode = abap_true ).
WRITE: |Edit result:"{ edit_result }"|.
