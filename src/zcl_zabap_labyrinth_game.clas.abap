CLASS zcl_zabap_labyrinth_game DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_settings,
        show_top_view TYPE abap_bool,
        width         TYPE i,
        height        TYPE i,
        fov           TYPE decfloat16,
      END OF t_settings.
    CONSTANTS:
      BEGIN OF c_function,
        left    TYPE string VALUE 'LEFT',
        forward TYPE string VALUE 'FORWARD',
        right   TYPE string VALUE 'RIGHT',
      END OF c_function.

    METHODS:
      start_new_game IMPORTING settings TYPE t_settings level_info TYPE zcl_zabap_labyrinth_level=>t_level OPTIONAL,
      pai IMPORTING function TYPE csequence,
      pbo,
      "! @parameter top_view | <p class="shorttext synchronized" lang="en">Must be supplied if show_top_view is ste to abap_true</p>
      set_containers IMPORTING fpp TYPE REF TO cl_gui_container top_view TYPE REF TO cl_gui_container OPTIONAL.

  PRIVATE SECTION.
    METHODS:
      get_colors RETURNING VALUE(colors) TYPE zcl_zabap_labyrinth_renderer=>tt_colors.
    DATA:
      settings TYPE t_settings,
      level    TYPE REF TO zcl_zabap_labyrinth_level,
      renderer TYPE REF TO zcl_zabap_labyrinth_renderer,
      BEGIN OF containers,
        fpp              TYPE REF TO cl_gui_container,
        top_view         TYPE REF TO cl_gui_container,
        fpp_picture      TYPE REF TO cl_gui_picture,
        top_view_picture TYPE REF TO cl_gui_picture,
      END OF containers.
ENDCLASS.



CLASS zcl_zabap_labyrinth_game IMPLEMENTATION.
  METHOD start_new_game.
    me->settings = settings.
    DATA(level_to_load) = COND #( WHEN NOT level_info IS INITIAL THEN level_info ELSE zcl_zabap_labyrinth_level_par=>get_level_easy( ) ).
    level = NEW #( level_to_load ).
    renderer = NEW #( settings = VALUE #( BASE CORRESPONDING #( settings ) colors = get_colors( ) ) level = level ).
  ENDMETHOD.


  METHOD pai.
    CASE function.
      WHEN c_function-left. level->turn_left( ).
      WHEN c_function-forward. level->try_move_forward( ).
      WHEN c_function-right. level->turn_right( ).
    ENDCASE.

    IF level->has_player_won( ).
      MESSAGE |You have won!| TYPE 'I'.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDMETHOD.

  METHOD pbo.
    IF settings-show_top_view = abap_true.
      IF containers-top_view_picture IS BOUND.
        containers-top_view_picture->load_picture_from_url_async( NEW zcl_zabap_bitmap_file( renderer->render_top_view( ) )->get_image_url( ) ).
      ELSE.
        containers-top_view_picture = NEW zcl_zabap_bitmap_file( renderer->render_top_view( ) )->display_in_container( containers-top_view ).
      ENDIF.
    ENDIF.

    IF containers-fpp_picture IS BOUND.
      containers-fpp_picture->load_picture_from_url_async( NEW zcl_zabap_bitmap_file( renderer->render_fpp( ) )->get_image_url( ) ).
    ELSE.
      containers-fpp_picture = NEW zcl_zabap_bitmap_file( renderer->render_fpp( ) )->display_in_container( containers-fpp ).
    ENDIF.
  ENDMETHOD.

  METHOD set_containers.
    containers-fpp = fpp.
    containers-top_view = top_view.
  ENDMETHOD.

  METHOD get_colors.
    INSERT VALUE #( field = zcl_zabap_labyrinth_level=>c_field-exit color = VALUE #( red = 220 green = 220 ) color_dimmed = VALUE #( red = 200 green = 200 ) ) INTO TABLE colors.
    INSERT VALUE #( field = zcl_zabap_labyrinth_level=>c_field-wall color = VALUE #( red = 40 green = 220 blue = 40 ) color_dimmed = VALUE #( red = 30 green = 200 blue = 30 ) ) INTO TABLE colors.
    INSERT VALUE #( field = zcl_zabap_labyrinth_level=>c_field-empty color = VALUE #( red = 80 green = 30 ) ) INTO TABLE colors.
    INSERT VALUE #( field = zcl_zabap_labyrinth_level=>c_field-player color = VALUE #( red = 200 ) color_dimmed = VALUE #( red = 150 ) ) INTO TABLE colors.

    INSERT VALUE #( field = zcl_zabap_labyrinth_renderer=>c_add_fields-sky color = VALUE #( red = 80 green = 190 blue = 210 ) ) INTO TABLE colors.
    INSERT VALUE #( field = zcl_zabap_labyrinth_renderer=>c_add_fields-ground color = VALUE #( red = 80 green = 30 ) ) INTO TABLE colors.
  ENDMETHOD.

ENDCLASS.
