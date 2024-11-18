"! <p class="shorttext synchronized" lang="en">Handler for screen events of function group <em>zea_alv_screen</em></p>
INTERFACE zif_ea_screen_handler PUBLIC.
  METHODS:
    pbo DEFAULT IGNORE,
    pai DEFAULT IGNORE IMPORTING VALUE(command) TYPE sy-ucomm.
ENDINTERFACE.
