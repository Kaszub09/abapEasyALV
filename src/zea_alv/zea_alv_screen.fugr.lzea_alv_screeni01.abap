*----------------------------------------------------------------------*
***INCLUDE LZMK_SCREEN_WITH_CONTAINERI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command INPUT.
  IF handler IS BOUND.
    DATA(command_converted) = sy-ucomm.
    IF command_converted CP 'DYNAMIC_*'.
      command_converted = commands[ CONV i( replace( val = command_converted sub = 'DYNAMIC_' with = ||  ) ) ]-command.
    ENDIF.

    handler->pai( command_converted ).
  ENDIF.
ENDMODULE.
