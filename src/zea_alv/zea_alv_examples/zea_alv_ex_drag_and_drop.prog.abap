*&---------------------------------------------------------------------*
*& Report zea_alv_ex_add_functions
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zea_alv_ex_drag_and_drop.

CLASS lcl_drag_drop_object_info DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    DATA:
        info TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_drag_drop_object_info IMPLEMENTATION.

ENDCLASS.

CLASS lcl_alv DEFINITION CREATE PUBLIC INHERITING FROM zcl_ea_alv_table.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS:
      on_drag REDEFINITION,
      on_drop REDEFINITION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_alv IMPLEMENTATION.

  METHOD on_drag.
    FIELD-SYMBOLS <table> TYPE table.
    ASSIGN data_table_ref->* TO <table>.
    ASSIGN COMPONENT e_column-fieldname OF STRUCTURE <table>[ e_row-index ] TO FIELD-SYMBOL(<value>).

    MESSAGE |Object picked on r={ e_row-index }; c={ e_column-fieldname }; Value={ <value> }| TYPE 'I'.

    DATA(object) = NEW lcl_drag_drop_object_info( ).
    object->info = <value>.
    e_dragdropobj->object = object.
  ENDMETHOD.

  METHOD on_drop.
    IF e_dragdropobj->object IS NOT INSTANCE OF lcl_drag_drop_object_info.
      RETURN.
    ENDIF.
    DATA(object) = CAST lcl_drag_drop_object_info( e_dragdropobj->object ).
    MESSAGE |Object dropped on r={ e_row-index }; c={ e_column-fieldname }; Value of object = { object->info }| TYPE 'S'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  SELECT * FROM sflight INTO TABLE @DATA(sflight).

  DATA(alv) = NEW lcl_alv(  ).
  alv->set_data( REF #( sflight ) ).
  alv->drag_drop->add( flavor = 'ABC' dragsrc = abap_true droptarget = abap_true ). "Enable drag and drop events
  alv->display_data( ).
