REPORT zabap_games_labyrinth.

"---------------------------------------------------------------------
" SELECTION SCREEN
"---------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-s01.
PARAMETERS:
  p_easy   RADIOBUTTON GROUP l,
  p_medium RADIOBUTTON GROUP l,
  p_custom RADIOBUTTON GROUP l,
  p_file   TYPE string.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-s02.
PARAMETERS: p_showtv AS CHECKBOX DEFAULT abap_true,
            p_width  TYPE i DEFAULT 80,
            p_height TYPE i DEFAULT 48,
            p_fov    TYPE decfloat16 DEFAULT '0.66'.
SELECTION-SCREEN END OF BLOCK b02.


"---------------------------------------------------------------------
" HTML event handler
"---------------------------------------------------------------------
CLASS lcl_html_event_handler DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      at_click FOR EVENT sapevent OF cl_abap_browser IMPORTING action.
ENDCLASS.
CLASS lcl_html_event_handler IMPLEMENTATION.
  METHOD at_click.
    cl_gui_cfw=>set_new_ok_code( SWITCH #( action WHEN 37 THEN 'LEFT' WHEN 38 THEN 'FORWARD' WHEN 39 THEN 'RIGHT' ) ).
  ENDMETHOD.
ENDCLASS.


"---------------------------------------------------------------------
" MAIN PROGRAM
"---------------------------------------------------------------------

START-OF-SELECTION.
  "Start game
  DATA(settings) = VALUE zcl_zabap_labyrinth_game=>t_settings( show_top_view = p_showtv height = p_height width = p_width fov = p_fov ).
  DATA(game) = NEW zcl_zabap_labyrinth_game( ).
  IF p_easy = abap_true.
    game->start_new_game( settings = settings level_info = zcl_zabap_labyrinth_level_par=>get_level_easy( ) ).
  ELSEIF p_medium = abap_true.
    game->start_new_game( settings = settings level_info = zcl_zabap_labyrinth_level_par=>get_level_medium( ) ).
  ELSEIF p_custom = abap_true.
    game->start_new_game( settings = settings level_info = zcl_zabap_labyrinth_level_par=>parse_file( p_file ) ).
  ENDIF.

  "Containers - 2nd column will allow easy resize
  DATA(gui_splitter) = NEW cl_gui_splitter_container( parent = NEW cl_gui_custom_container( 'CONTAINER' ) rows = 2 columns = 2 ).
  gui_splitter->set_column_width( id = 2 width = 1 ).
  gui_splitter->set_row_height( id = 1 height = 24 ).
  IF p_showtv = abap_true.
    game->set_containers( fpp = gui_splitter->get_container( row = 2 column = 1 ) top_view = gui_splitter->get_container( row = 1 column = 1 ) ).
  ELSE.
    game->set_containers( fpp = gui_splitter->get_container( row = 2 column = 1 ) ).
  ENDIF.

  "Empty container to catch key press events
  SET HANDLER lcl_html_event_handler=>at_click.
  DATA(html) = |<head><script type="text/javascript">| &
      |function okd(e)\{ c=window.event.keyCode; window.location='sapevent:'+c; \}| &
      |document.onkeydown = okd;</script>| &
      |</head><body></body>|.
  cl_abap_browser=>show_html( html_string = html modal = abap_false dialog = abap_false container = gui_splitter->get_container( row = 1 column = 2 ) ).

  CALL SCREEN 0001.


  "---------------------------------------------------------------------
  " MODULES
  "---------------------------------------------------------------------
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'GAME_CONTROLS'.
  game->pbo( ).
  cl_abap_browser=>show_html( html_string = html modal = abap_false dialog = abap_false  container = gui_splitter->get_container( row = 1 column = 2 ) ).
ENDMODULE.

MODULE user_command_0001 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'. LEAVE TO SCREEN 0.
    WHEN 'EXIT'. LEAVE PROGRAM.
    WHEN OTHERS. game->pai( sy-ucomm ).
  ENDCASE.
ENDMODULE.
