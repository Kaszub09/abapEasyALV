"! <p class="shorttext synchronized">Set event handler</p>
FUNCTION zea_screen_set_handler.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(EVENT_HANDLER) TYPE REF TO  ZIF_EA_SCREEN_HANDLER
*"----------------------------------------------------------------------
  handler = event_handler.
ENDFUNCTION.
