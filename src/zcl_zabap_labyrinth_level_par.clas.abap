CLASS zcl_zabap_labyrinth_level_par DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
        tt_string TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    CLASS-METHODS:
      parse_file IMPORTING full_file_path TYPE string RETURNING VALUE(level) TYPE zcl_zabap_labyrinth_level=>t_level,
      "!Expected file structure:
      "!<br/>#[comments]
      "!<br/>[rows],[columns],[player row], [player col],[player direction]
      "!<br/>[last map row = rows - 1]
      "!<br/>.
      "!<br/>.
      "!<br/>.
      "!<br/>[first map row = 0]
      "! <br/><br/>
      "! Symbols: 0,1,2,3 for player direction N,E,S,W
      "! 0 - empty, 1 - wall, 2 - exit
      parse_text_to_level IMPORTING text TYPE tt_string RETURNING VALUE(level) TYPE zcl_zabap_labyrinth_level=>t_level,
      get_level_easy RETURNING VALUE(level) TYPE zcl_zabap_labyrinth_level=>t_level,
      get_level_medium RETURNING VALUE(level) TYPE zcl_zabap_labyrinth_level=>t_level.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_zabap_labyrinth_level_par IMPLEMENTATION.

  METHOD get_level_easy.
    DATA string_table TYPE tt_string.

    APPEND |#EASY| TO string_table.
    APPEND |10,10,1,1,0| TO string_table.
    APPEND |1111111111| TO string_table.
    APPEND |1010000021| TO string_table.
    APPEND |1010100101| TO string_table.
    APPEND |1010101101| TO string_table.
    APPEND |1000101101| TO string_table.
    APPEND |1000100101| TO string_table.
    APPEND |1010101001| TO string_table.
    APPEND |1010101001| TO string_table.
    APPEND |1010000001| TO string_table.
    APPEND |1111111111| TO string_table.

    level = parse_text_to_level( string_table ).
  ENDMETHOD.

  METHOD get_level_medium.
    DATA string_table TYPE tt_string.

    APPEND |#MEDIUM| TO string_table.
    APPEND |14,20,12,17,3| TO string_table.
    APPEND |11111111111111111111| TO string_table.
    APPEND |10000100010011001001| TO string_table.
    APPEND |10110001000110100011| TO string_table.
    APPEND |11010111010000000111| TO string_table.
    APPEND |11001000001110111111| TO string_table.
    APPEND |11100011111011100011| TO string_table.
    APPEND |11001011000011101111| TO string_table.
    APPEND |11100011101110000101| TO string_table.
    APPEND |10101011100000110001| TO string_table.
    APPEND |10001000001111111111| TO string_table.
    APPEND |11011011101110110001| TO string_table.
    APPEND |11001010101100000101| TO string_table.
    APPEND |10011110000001101121| TO string_table.
    APPEND |11111111111111111111| TO string_table.

    level = parse_text_to_level( string_table ).
  ENDMETHOD.

  METHOD parse_file.
    DATA string_table TYPE tt_string.
    cl_gui_frontend_services=>gui_upload( EXPORTING filename = full_file_path CHANGING data_tab = string_table ).
    level = parse_text_to_level( string_table ).
  ENDMETHOD.

  METHOD parse_text_to_level.
    LOOP AT text REFERENCE INTO DATA(line).
      IF line->*(1) = '#'.
        CONTINUE.
      ENDIF.

      "First non-comment row is header
      SPLIT line->* AT ',' INTO TABLE DATA(header).
      level-height = header[ 1 ].
      level-width = header[ 2 ].
      level-starting_position-row = header[ 3 ].
      level-starting_position-col = header[ 4 ].
      level-starting_position-dir = header[ 5 ].

      EXIT.
    ENDLOOP.

    DATA(row) = level-height - 1.
    DATA(index) = lines( text ) + 1.
    WHILE index >= 2 AND row >= 0.
      index = index - 1.
      line = REF #( text[ index ] ).
      IF line->*(1) = '#' OR condense( line->* ) = space. "Skip comments and empty lines
        CONTINUE.
      ENDIF.

      "Parse row to columns
      DATA(col) = 0.
      WHILE col < level-width.
        APPEND CONV int1( line->*+col(1) ) TO level-map.
        col = col + 1.
      ENDWHILE.

      row = row - 1.
    ENDWHILE.

  ENDMETHOD.

ENDCLASS.
