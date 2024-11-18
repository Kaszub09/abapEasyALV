FUNCTION zea_salv_set_functions.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(FUNCTIONS) TYPE
*"        ZCL_EA_SALV_TABLE_FUNCTIONS=>TT_FUNCTIONS
*"----------------------------------------------------------------------
  CLEAR: dynamic.

  LOOP AT functions REFERENCE INTO DATA(func).
    ASSIGN COMPONENT sy-tabix OF STRUCTURE dynamic TO FIELD-SYMBOL(<dynamic_func>).
    <dynamic_func> = func->description.
  ENDLOOP.
ENDFUNCTION.
