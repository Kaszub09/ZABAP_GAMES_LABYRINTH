CLASS zcl_zabap_labyrinth_level DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_player,
        row TYPE i,
        col TYPE i,
        dir TYPE c LENGTH 1,
      END OF t_player,
      BEGIN OF t_level,
        width             TYPE i,
        height            TYPE i,
        "! Indexed from 0. Bottom to top, left to right, like bitmap
        map               TYPE STANDARD TABLE OF int1 WITH EMPTY KEY,
        starting_position TYPE t_player,
      END OF t_level.

    CONSTANTS:
      BEGIN OF c_field,
        empty  TYPE int1 VALUE 0,
        wall   TYPE int1 VALUE 1,
        exit   TYPE int1 VALUE 2,
        player TYPE int1 VALUE 3,
      END OF c_field,
      BEGIN OF c_dir,
        north TYPE int1 VALUE 0,
        east  TYPE int1 VALUE 1,
        south TYPE int1 VALUE 2,
        west  TYPE int1 VALUE 3,
      END OF c_dir.

    METHODS:
      constructor IMPORTING level TYPE t_level,
      turn_left,
      turn_right,
      try_move_forward,
      has_player_won RETURNING VALUE(has_won) TYPE abap_bool.

    DATA:
      level  TYPE t_level READ-ONLY,
      player TYPE t_player READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_zabap_labyrinth_level IMPLEMENTATION.
  METHOD constructor.
    me->level = level.
    player = level-starting_position.
  ENDMETHOD.

  METHOD has_player_won.
    has_won = COND #( WHEN level-map[ player-row * level-width + player-col + 1 ] = c_field-exit THEN abap_true ELSE abap_false ).
  ENDMETHOD.

  METHOD try_move_forward.
    DATA(next_row) = player-row + SWITCH #( player-dir WHEN c_dir-north THEN 1 WHEN c_dir-south THEN -1 ELSE 0 ).
    DATA(next_col) = player-col + SWITCH #( player-dir WHEN c_dir-east THEN 1 WHEN c_dir-west THEN -1 ELSE 0 ).

    "Is out of bonds?
    IF next_row < 0 OR level-height < next_row OR next_col < 0 OR level-width < next_col.
      RETURN.
    ENDIF.

    "Can't move?
    IF level-map[ next_row * level-width + next_col + 1 ] = c_field-wall.
      RETURN.
    ENDIF.

    "Execute move
    player-row = next_row.
    player-col = next_col.
  ENDMETHOD.

  METHOD turn_left.
    player-dir = ( player-dir - 1 ) MOD 4.
  ENDMETHOD.

  METHOD turn_right.
    player-dir = ( player-dir + 1 ) MOD 4.
  ENDMETHOD.

ENDCLASS.
