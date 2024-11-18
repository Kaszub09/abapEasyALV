"! <p class="shorttext synchronized" lang="en">Easy ALV screen - for easy displaying stuff in container without creating new screen everytime. </p>
"! You need to create your own containers (e.g. cl_gui_docking_container) and destroy them after.
"! <br/>TAGS: display; screen
CLASS zcl_ea_screen DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_command,
        command     TYPE syst_ucomm,
        description TYPE smp_dyntxt,
      END OF t_command,
      tt_command TYPE STANDARD TABLE OF t_command WITH EMPTY KEY.

    CONSTANTS:
      c_program_name           TYPE syrepid VALUE 'SAPLZEA_ALV_SCREEN',
      c_screen_with_toolbar    TYPE sydynnr VALUE '0001',
      c_screen_without_toolbar TYPE sydynnr VALUE '0002'.

    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Set to catch PBO/PAI events of screen</p>
      set_event_handler IMPORTING handler TYPE REF TO zif_ea_screen_handler,
      clear_event_handler,
      "! <p class="shorttext synchronized" lang="en">Create e.g. docking container using <em>c_program_name</em> and <em>c_screen</em>, then free it afterwards.</p>
      "! If you are displaying screen as popup, you should create all containers etc. inside PBO function of interface (and remember to destroy them after).
      "! <br/>You can use statues and headers of program inside PBO event.
      "! @parameter commands | <p class="shorttext synchronized" lang="en">If supplied, calls screen <em>c_screen_with_toolbar</em>. Otherwise <em>c_screen_without_toolbar</em></p>
      display_screen IMPORTING commands TYPE tt_command OPTIONAL start_column TYPE i DEFAULT 0 end_column TYPE i DEFAULT 128
                               start_line TYPE i DEFAULT 0 end_line TYPE i DEFAULT 24.
ENDCLASS.

CLASS zcl_ea_screen IMPLEMENTATION.
  METHOD clear_event_handler.
    CALL FUNCTION 'ZEA_SCREEN_CLEAR_HANDLER'.
  ENDMETHOD.

  METHOD display_screen.
    CALL FUNCTION 'ZEA_SCREEN_DISPLAY'
      EXPORTING
        dynamic_commands = commands
        start_column     = start_column
        end_column       = end_column
        start_line       = start_line
        end_line         = end_line.
  ENDMETHOD.

  METHOD set_event_handler.
    CALL FUNCTION 'ZEA_SCREEN_SET_HANDLER' EXPORTING event_handler = handler.
  ENDMETHOD.
ENDCLASS.
