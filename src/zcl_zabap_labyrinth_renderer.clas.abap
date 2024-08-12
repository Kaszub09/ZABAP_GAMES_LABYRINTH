CLASS zcl_zabap_labyrinth_renderer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_colors,
        field        LIKE zcl_zabap_labyrinth_level=>c_field-empty,
        color        TYPE zcl_zabap_bitmap=>t_pixel,
        "! Secondary color for FPP, e.g. to differentiate between front and side of wall
        color_dimmed TYPE zcl_zabap_bitmap=>t_pixel,
      END OF t_colors,
      tt_colors TYPE HASHED TABLE OF t_colors WITH UNIQUE KEY field,
      BEGIN OF t_settings,
        width  TYPE i,
        height TYPE i,
        fov    TYPE decfloat16,
        colors TYPE tt_colors,
      END OF t_settings.

    CONSTANTS:
      BEGIN OF c_add_fields,
        sky    LIKE zcl_zabap_labyrinth_level=>c_field-empty VALUE 100,
        ground LIKE zcl_zabap_labyrinth_level=>c_field-empty VALUE 101,
      END OF c_add_fields.

    METHODS:
      constructor IMPORTING settings TYPE t_settings level TYPE REF TO zcl_zabap_labyrinth_level,
      render_top_view RETURNING VALUE(bitmap) TYPE REF TO zcl_zabap_bitmap,
      render_fpp RETURNING VALUE(bitmap) TYPE REF TO zcl_zabap_bitmap.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_vector,
        x TYPE decfloat34,
        y TYPE decfloat34,
      END OF t_vector,
      BEGIN OF t_vector_int,
        x TYPE i,
        y TYPE i,
      END OF t_vector_int,
      BEGIN OF t_raycast_result,
        field      LIKE zcl_zabap_labyrinth_level=>c_field-empty,
        height     TYPE i,
        is_ns_side TYPE abap_bool,
      END OF t_raycast_result,
      tt_raycast_result TYPE STANDARD TABLE OF t_raycast_result WITH EMPTY KEY.
    METHODS:
      run_raycast RETURNING VALUE(raycast) TYPE tt_raycast_result.
    DATA:
      settings TYPE t_settings,
      colors   TYPE tt_colors,
      level    TYPE REF TO zcl_zabap_labyrinth_level,
      top_view TYPE REF TO zcl_zabap_bitmap,
      fpp      TYPE REF TO zcl_zabap_bitmap.
ENDCLASS.



CLASS zcl_zabap_labyrinth_renderer IMPLEMENTATION.
  METHOD constructor.
    me->settings = settings.
    me->level = level.

    "We need to draw it 2x bigger than field in order to mark player direction (so player can't be just one pixel)
    top_view = zcl_zabap_bitmap=>create_new( height = level->level-height * 2 width = level->level-width * 2 ).
    fpp = zcl_zabap_bitmap=>create_new( height = settings-height width = settings-width ).
  ENDMETHOD.

  METHOD render_top_view.
    DATA color_info TYPE t_colors.

    "Draw map without player
    "TODO compare performance REF/DATA
    LOOP AT level->level-map REFERENCE INTO DATA(field).
      "Remember, we are drawing map scaled up by 2
      DATA(index) = sy-tabix - 1.
      DATA(col) = 2 * ( index MOD level->level-width ).
      DATA(row) = 2 * ( ( index - index MOD level->level-width ) / level->level-width ). "Since int division is rounded. 7/4 = 2. WTF, SAP?

      color_info = settings-colors[ field = field->* ].

      "Color 2x2 square .
      top_view->set_pixel( row = row col = col pixel = color_info-color ).
      top_view->set_pixel( row = row col = col + 1 pixel = color_info-color ).
      top_view->set_pixel( row = row + 1 col = col pixel = color_info-color ).
      top_view->set_pixel( row = row + 1 col = col + 1 pixel = color_info-color ).
    ENDLOOP.

    "Draw player
    color_info = settings-colors[ field = zcl_zabap_labyrinth_level=>c_field-player ].
    top_view->set_pixel( row = 2 * level->player-row col = 2 * level->player-col pixel = color_info-color ).
    top_view->set_pixel( row = 2 * level->player-row col = 2 * level->player-col + 1 pixel = color_info-color ).
    top_view->set_pixel( row = 2 * level->player-row + 1 col = 2 * level->player-col pixel = color_info-color ).
    top_view->set_pixel( row = 2 * level->player-row + 1 col = 2 * level->player-col + 1 pixel = color_info-color ).
    CASE level->player-dir.
      WHEN zcl_zabap_labyrinth_level=>c_dir-north.
        top_view->set_pixel( row = 2 * level->player-row col = 2 * level->player-col pixel = color_info-color_dimmed ).
        top_view->set_pixel( row = 2 * level->player-row col = 2 * level->player-col + 1 pixel = color_info-color_dimmed ).
      WHEN zcl_zabap_labyrinth_level=>c_dir-east.
        top_view->set_pixel( row = 2 * level->player-row col = 2 * level->player-col pixel = color_info-color_dimmed ).
        top_view->set_pixel( row = 2 * level->player-row + 1 col = 2 * level->player-col pixel = color_info-color_dimmed ).
      WHEN zcl_zabap_labyrinth_level=>c_dir-south.
        top_view->set_pixel( row = 2 * level->player-row + 1 col = 2 * level->player-col pixel = color_info-color_dimmed ).
        top_view->set_pixel( row = 2 * level->player-row + 1 col = 2 * level->player-col + 1 pixel = color_info-color_dimmed ).
      WHEN zcl_zabap_labyrinth_level=>c_dir-west.
        top_view->set_pixel( row = 2 * level->player-row col = 2 * level->player-col + 1 pixel = color_info-color_dimmed ).
        top_view->set_pixel( row = 2 * level->player-row + 1 col = 2 * level->player-col + 1 pixel = color_info-color_dimmed ).
    ENDCASE.

    bitmap = top_view.
  ENDMETHOD.

  METHOD render_fpp.
    DATA(raycast) = run_raycast( ).

    DATA(col) = 0.
    WHILE col < settings-width.
      DATA(ray) = REF #( raycast[ col + 1 ] ).
      DATA(distance_object_to_edge) = ( settings-height - ray->height ) / 2.
      DATA(row) = 0.

      WHILE row < settings-height.
        IF row < distance_object_to_edge.
          fpp->set_pixel( row = row col = col pixel = settings-colors[ field = c_add_fields-ground ]-color ).
        ELSEIF row >= settings-height - distance_object_to_edge.
          fpp->set_pixel( row = row col = col pixel = settings-colors[ field = c_add_fields-sky ]-color ).
        ELSE.
          fpp->set_pixel( row = row col = col pixel = COND #(
              WHEN ray->is_ns_side = abap_true THEN settings-colors[ field = ray->field ]-color
              ELSE settings-colors[ field = ray->field ]-color_dimmed ) ).
        ENDIF.
        row = row + 1.
      ENDWHILE.
      col = col + 1.
    ENDWHILE.

    bitmap = fpp.
  ENDMETHOD.

  METHOD run_raycast.
    "Credit - https://lodev.org/cgtutor/raycasting.html
    "y = row, x = col
    "Player is located in leftdown corner - move him so he can see whole tile before him (e.g. place him in the middle, then move half-tile back)
    DATA(player) = VALUE t_vector( x = CONV decfloat34( level->player-col ) + SWITCH decfloat34( level->player-dir WHEN level->c_dir-north OR level->c_dir-south THEN '0.5' WHEN level->c_dir-east THEN 0 WHEN level->c_dir-west THEN 1 )
                                   y = CONV decfloat34( level->player-row ) + SWITCH decfloat34( level->player-dir WHEN level->c_dir-east OR level->c_dir-west THEN '0.5' WHEN level->c_dir-south THEN 1 WHEN level->c_dir-north THEN 0 ) ).
    "Determine camera direction
    DATA(dir) = VALUE t_vector( x = SWITCH decfloat34( level->player-dir WHEN level->c_dir-north OR level->c_dir-south THEN 0 WHEN level->c_dir-east THEN 1 WHEN level->c_dir-west THEN -1 )
                                y = SWITCH decfloat34( level->player-dir WHEN level->c_dir-east OR level->c_dir-west THEN 0 WHEN level->c_dir-south THEN -1 WHEN level->c_dir-north THEN 1 ) ).

    DATA(plane) = VALUE t_vector( x = settings-fov * dir-y y = -1 * settings-fov * dir-x ).

    DATA(pixel) = 1.
    WHILE pixel <= settings-width.
      "x-coordinate in camera plane (imagine we slice camera plane into [width] parts - we calculate which part we process now)
      "we skip y as we will just calculate height based on distance
      DATA(cam) = VALUE t_vector( x = 2 * ( pixel - 1 ) / settings-width  - 1 ).

      "calculate ray position and direction (from player to the corresponding point on camera plane)
      DATA(ray_dir) = VALUE t_vector( x = dir-x + plane-x * cam-x y = dir-y + plane-y * cam-x ).

      "Tile of the map we're in
      DATA(map) = VALUE t_vector( x = level->player-col y = level->player-row ).

      "length of ray from current position to next x or y-side
      DATA: side_dist TYPE t_vector.

      "length of ray from one x or y-side to next x or y-side - we set some big value as infinity for parallel walls
      DATA(delta_dist) = VALUE t_vector( x = COND #( WHEN ray_dir-x = 0 THEN '10000000000000' ELSE abs( 1 / ray_dir-x ) )
                                        y = COND #( WHEN ray_dir-y = 0 THEN '10000000000000' ELSE abs( 1 / ray_dir-y ) ) ).
      "DATA(delta_dist) = VALUE t_vector( x = COND #( WHEN ray_dir-x = 0 THEN '10000000000000' ELSE sqrt( 1 + ( ray_dir-y * ray_dir-y ) / ( ray_dir-x * ray_dir-x ) ) )
      "                                   y = COND #( WHEN ray_dir-y = 0 THEN '10000000000000' ELSE sqrt( 1 + ( ray_dir-x * ray_dir-x ) / ( ray_dir-y * ray_dir-y ) ) ) ).

      "what direction to step in (either +1 or -1)
      DATA: step TYPE t_vector_int.
      DATA is_ns_side TYPE abap_bool. "was a NS or a EW wall hit?
      DATA(hit) = abap_false. "was there a wall hit?

      "calculate step and initial side_dist - we look from position of bottom-left corner of square
      IF ray_dir-x < 0.
        step-x = -1.
        side_dist-x = ( player-x - map-x ) * delta_dist-x.
      ELSE.
        step-x = 1.
        side_dist-x = ( map-x + 1 - player-x ) * delta_dist-x.
      ENDIF.

      IF ray_dir-y < 0.
        step-y = -1.
        side_dist-y = ( player-y - map-y ) * delta_dist-y.
      ELSE.
        step-y = 1.
        side_dist-y = ( map-y + 1 - player-y ) * delta_dist-y.
      ENDIF.

      "DDA
      WHILE hit = abap_false.
        IF side_dist-x < side_dist-y.
          side_dist-x = side_dist-x + delta_dist-x.
          map-x = map-x + step-x.
          is_ns_side = abap_true.

        ELSE.
          side_dist-y = side_dist-y + delta_dist-y.
          map-y = map-y + step-y.
          is_ns_side = abap_false.
        ENDIF.

        "Check in bounds in case we missed a wall (but we should never) to avoid endless loop
        IF map-y < 0 OR level->level-height < map-y OR map-x < 0 OR level->level-width < map-x.
          hit = abap_true.
          EXIT.
        ENDIF.

        "check if ray has hit a wall
        DATA(field) = level->level-map[ map-y * level->level-width + map-x + 1 ].
        IF field = level->c_field-wall OR field = level->c_field-exit.
          hit = abap_true.
        ENDIF.
      ENDWHILE.

      DATA(perp_wall_dist) = abs( COND decfloat34( WHEN is_ns_side = abap_true THEN side_dist-x - delta_dist-x ELSE side_dist-y - delta_dist-y ) ).

      "Calculate height of line to draw on screen
      DATA(line_height) = COND i( WHEN perp_wall_dist <= 1 THEN settings-height ELSE settings-height / perp_wall_dist ).
      APPEND VALUE #( field = field height = line_height is_ns_side = is_ns_side ) TO raycast.

      pixel = pixel + 1.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.
