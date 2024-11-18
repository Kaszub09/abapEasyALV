FUNCTION zea_screen_display.
*"----------------------------------------------------------------------
*"*"Lokalny interfejs:
*"  IMPORTING
*"     REFERENCE(DYNAMIC_COMMANDS) TYPE  ZCL_EA_SCREEN=>TT_COMMAND
*"       OPTIONAL
*"     REFERENCE(START_COLUMN) TYPE  I
*"     REFERENCE(END_COLUMN) TYPE  I
*"     REFERENCE(START_LINE) TYPE  I
*"     REFERENCE(END_LINE) TYPE  I
*"----------------------------------------------------------------------
  commands = dynamic_commands.
  DATA(screen) = 1. "Screen with toolbar

  "Map to dynamic commands inside function module
  CLEAR dynamic.
  LOOP AT commands REFERENCE INTO DATA(command).
    ASSIGN COMPONENT |DYNAMIC_{ sy-tabix }| OF STRUCTURE dynamic TO FIELD-SYMBOL(<dynamic>).
    <dynamic> = command->description.
  ENDLOOP.

  IF lines( commands ) = 0.
    screen = 2.  "Screen with no toolbar
  ENDIF.

  CALL SCREEN screen STARTING AT start_column start_line ENDING AT end_column end_line.
ENDFUNCTION.
