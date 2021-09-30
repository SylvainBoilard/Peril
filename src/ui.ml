open Utils
open Bigarray

type army_count_selector = {
    center: Vec2.t;
    color_suite: Color.suite;
    selector_count: int;
    enabled_count: int;
    mutable activated: int;
    mutable anim_time: float;
    vertex_buffer: GL.buffer;
    texture: GL.texture;
    base_texture_coords: Vec2.t;
  }

let make_army_count_selector_template selector_count texture base_texture_coords =
  { center = Vec2.zero;
    color_suite = Color.(make_suite (hsla_of_name Black));
    selector_count;
    enabled_count = 0;
    activated = -1;
    anim_time = 0.0;
    vertex_buffer = GL.genBuffer ();
    texture;
    base_texture_coords }

let make_army_count_selector_from_template selector_data center color_suite enabled_count =
  { selector_data with center; color_suite; enabled_count }

let find_hovered_selector selector_data cursor_coords =
  let bot_center = selector_data.center.y -. float_of_int (selector_data.selector_count - 1) *. 0.128 in
  let rec loop = function
    | -1 -> -1
    | i ->
       let y = bot_center +. float_of_int i *. 0.256 in
       if Vec2.(sqr_mag (sub cursor_coords { selector_data.center with y })) <= 0.112 *. 0.112
       then i
       else loop (i - 1)
  in
  loop (selector_data.selector_count - 1)

let draw_army_count_selector basic_shader selector_data cursor_coords =
  let anim_time_geom =
    if selector_data.activated = -1
    then ease_out selector_data.anim_time
    else ease_in_out selector_data.anim_time
  in
  let anim_time_colo =
    if selector_data.activated = -1
    then ease_in selector_data.anim_time
    else ease_in_out selector_data.anim_time
  in
  let bot = selector_data.center.y -. float_of_int (selector_data.selector_count - 1) *. 0.128 *. anim_time_geom in
  let buffer_data =
    let left, right = selector_data.center.x -. 0.128, selector_data.center.x +. 0.128 in
    let top = selector_data.center.y +. float_of_int (selector_data.selector_count - 1) *. 0.128 *. anim_time_geom in
    [|
      (* background *)
      right; top +. 0.128;   0.375; 0.75 ;   1.0; 1.0; 1.0; 1.0;
      left ; top +. 0.128;   0.25 ; 0.75 ;   1.0; 1.0; 1.0; 1.0;
      right; top         ;   0.375; 0.875;   1.0; 1.0; 1.0; 1.0;
      left ; top         ;   0.25 ; 0.875;   1.0; 1.0; 1.0; 1.0;
      right; bot         ;   0.375; 0.875;   1.0; 1.0; 1.0; 1.0;
      left ; bot         ;   0.25 ; 0.875;   1.0; 1.0; 1.0; 1.0;
      right; bot -. 0.128;   0.375; 1.0  ;   1.0; 1.0; 1.0; 1.0;
      left ; bot -. 0.128;   0.25 ; 1.0  ;   1.0; 1.0; 1.0; 1.0;
      (* selector *) (* TODO: dont update this. *)
       0.128;  0.128;   0.125; 0.0 ;   1.0; 1.0; 1.0; 1.0;
      -0.128;  0.128;   0.0  ; 0.0 ;   1.0; 1.0; 1.0; 1.0;
      -0.128; -0.128;   0.0  ; 0.25;   1.0; 1.0; 1.0; 1.0;
       0.128; -0.128;   0.125; 0.25;   1.0; 1.0; 1.0; 1.0;
    |] |> Array1.of_array Float32 C_layout
  in
  GL.bindBuffer GL.ArrayBuffer selector_data.vertex_buffer;
  GL.bufferData GL.ArrayBuffer buffer_data GL.StreamDraw;
  let bg_l = if selector_data.activated = -1 then 0.75 else Float.lerp 0.5 0.75 anim_time_colo in
  let bg_a = if selector_data.activated = -1 then 0.5 *. anim_time_colo else 0.5 in
  GL.uniform4f basic_shader.ambient_color_location bg_l bg_l bg_l bg_a;
  GL.uniform2f basic_shader.vertex_coords_offset_location 0.0 0.0;
  GL.uniform2f basic_shader.texture_coords_offset_location 0.0 0.0;
  Render.draw_basic basic_shader selector_data.texture selector_data.vertex_buffer GL.TriangleStrip 0 8;
  let hovered_selector = find_hovered_selector selector_data cursor_coords in
  for i = 0 to selector_data.selector_count - 1 do
    let i_f = float_of_int i in
    let y = bot +. i_f *. 0.256 *. anim_time_geom in
    let tx = selector_data.base_texture_coords.x +. i_f *. 0.125 in
    let c =
      if i = selector_data.activated then
        selector_data.color_suite.normal
      else if i + 1 > selector_data.enabled_count then
        selector_data.color_suite.desaturated
      else if i = hovered_selector then
        selector_data.color_suite.brighter
      else
        selector_data.color_suite.darker
    in
    let a = if i = selector_data.activated then 1.0 else anim_time_colo in
    GL.uniform4f basic_shader.ambient_color_location c.r c.g c.b a;
    GL.uniform2f basic_shader.vertex_coords_offset_location selector_data.center.x y;
    GL.uniform2f basic_shader.texture_coords_offset_location tx selector_data.base_texture_coords.y;
    Render.draw_basic basic_shader selector_data.texture selector_data.vertex_buffer GL.TriangleFan 8 4
  done

type battle_resolution = {
    vertex_buffer: GL.buffer;
    elem_buffer: GL.buffer;
    arrow_buffer: GL.buffer;
    texture: GL.texture;
    mutable anim_time: float;
  }

let make_battle_resolution texture =
  let vertex_buffer_data =
    [|(* background *)
       0.532;  0.532;   0.375 ; 0.75 ;   0.5; 0.5; 0.5; 0.5;
       0.404;  0.532;   0.3125; 0.75 ;   0.5; 0.5; 0.5; 0.5;
      -0.404;  0.532;   0.3125; 0.75 ;   0.5; 0.5; 0.5; 0.5;
      -0.532;  0.532;   0.25  ; 0.75 ;   0.5; 0.5; 0.5; 0.5;
       0.532;  0.404;   0.375 ; 0.875;   0.5; 0.5; 0.5; 0.5;
       0.404;  0.404;   0.3125; 0.875;   0.5; 0.5; 0.5; 0.5;
      -0.404;  0.404;   0.3125; 0.875;   0.5; 0.5; 0.5; 0.5;
      -0.532;  0.404;   0.25  ; 0.875;   0.5; 0.5; 0.5; 0.5;
       0.532; -0.404;   0.375 ; 0.875;   0.5; 0.5; 0.5; 0.5;
       0.404; -0.404;   0.3125; 0.875;   0.5; 0.5; 0.5; 0.5;
      -0.404; -0.404;   0.3125; 0.875;   0.5; 0.5; 0.5; 0.5;
      -0.532; -0.404;   0.25  ; 0.875;   0.5; 0.5; 0.5; 0.5;
       0.532; -0.532;   0.375 ; 1.0  ;   0.5; 0.5; 0.5; 0.5;
       0.404; -0.532;   0.3125; 1.0  ;   0.5; 0.5; 0.5; 0.5;
      -0.404; -0.532;   0.3125; 1.0  ;   0.5; 0.5; 0.5; 0.5;
      -0.532; -0.532;   0.25  ; 1.0  ;   0.5; 0.5; 0.5; 0.5;
      (* dice *)
       0.128;  0.128;   0.125 ; 0.0  ;   1.0; 1.0; 1.0; 1.0;
      -0.128;  0.128;   0.0   ; 0.0  ;   1.0; 1.0; 1.0; 1.0;
      -0.128; -0.128;   0.0   ; 0.25 ;   1.0; 1.0; 1.0; 1.0;
       0.128; -0.128;   0.125 ; 0.25 ;   1.0; 1.0; 1.0; 1.0;
    |] |> Array1.of_array Float32 C_layout
  in
  let elem_buffer_data =
    [|(* background stripes *)
      4; 0; 5; 1; 6; 2; 7; 3; (* top *)
      8; 4; 9; 5; 10; 6; 11; 7; (* mid *)
      12; 8; 13; 9; 14; 10; 15; 11; (* bottom *)
    |] |> Array1.of_array Int16_unsigned C_layout
  in
  let vertex_buffer = GL.genBuffer () in
  GL.bindBuffer GL.ArrayBuffer vertex_buffer;
  GL.bufferData GL.ArrayBuffer vertex_buffer_data GL.StaticDraw;
  let elem_buffer = GL.genBuffer () in
  GL.bindBuffer GL.ArrayBuffer elem_buffer;
  GL.bufferData GL.ArrayBuffer elem_buffer_data GL.StaticDraw;
  { vertex_buffer; elem_buffer; arrow_buffer = GL.genBuffer (); texture; anim_time = 0.0 }

let draw_battle_resolution
      basic_shader battle_resolution_data dice_points dice_order
      attacking_armies defending_armies =
  Render.draw_basic_multi
    basic_shader battle_resolution_data.texture battle_resolution_data.vertex_buffer
    ~elem_buffer:battle_resolution_data.elem_buffer TriangleStrip [0, 8; 8, 8; 16, 8];
  let dice_t = Float.clamp 0.0 1.0 ((battle_resolution_data.anim_time -. 1.1) *. 2.0) in
  let dice_t_io = ease_in_out dice_t in
  let dice_t_i_f = ease_in (min 1.0 (dice_t *. 2.0)) in
  let arrow_t = Float.clamp 0.0 1.0 ((battle_resolution_data.anim_time -. 1.3) *. 2.0) in
  let arrow_t_io = ease_in_out arrow_t in
  let arrow_t_i_f = ease_in (min 1.0 (arrow_t *. 3.0)) in
  let min_armies = min attacking_armies defending_armies in
  for i = 0 to min_armies - 1 do
    let x = Float.lerp (-0.172) 0.140 arrow_t_io in
    let y = float_of_int (i - 1) *. -0.3 in
    let buffer_data =
      Array1.of_array Float32 C_layout @@
        if dice_points.(i) > dice_points.(i + 3) then
          [|   x         ; y +. 0.032;   0.390625; 0.5   ;   1.0; 1.0; 1.0; arrow_t_i_f;
              -0.172     ; y +. 0.032;   0.375   ; 0.5   ;   1.0; 1.0; 1.0; arrow_t_i_f;
              -0.172     ; y -. 0.032;   0.375   ; 0.5625;   1.0; 1.0; 1.0; arrow_t_i_f;
               x         ; y -. 0.032;   0.390625; 0.5625;   1.0; 1.0; 1.0; arrow_t_i_f;
               x +. 0.032; y -. 0.032;   0.40625 ; 0.5625;   1.0; 1.0; 1.0; arrow_t_i_f;
               x +. 0.032; y +. 0.032;   0.40625 ; 0.5   ;   1.0; 1.0; 1.0; arrow_t_i_f |]
        else
          [| -.x         ; y +. 0.032;   0.390625; 0.5625;   1.0; 1.0; 1.0; arrow_t_i_f;
             -.x -. 0.032; y +. 0.032;   0.40625 ; 0.5625;   1.0; 1.0; 1.0; arrow_t_i_f;
             -.x -. 0.032; y -. 0.032;   0.40625 ; 0.625 ;   1.0; 1.0; 1.0; arrow_t_i_f;
             -.x         ; y -. 0.032;   0.390625; 0.625 ;   1.0; 1.0; 1.0; arrow_t_i_f;
               0.172     ; y -. 0.032;   0.375   ; 0.625 ;   1.0; 1.0; 1.0; arrow_t_i_f;
               0.172     ; y +. 0.032;   0.375   ; 0.5625;   1.0; 1.0; 1.0; arrow_t_i_f |]
    in
    GL.bindBuffer GL.ArrayBuffer battle_resolution_data.arrow_buffer;
    GL.bufferData GL.ArrayBuffer buffer_data StreamDraw;
    Render.draw_basic basic_shader battle_resolution_data.texture battle_resolution_data.arrow_buffer GL.TriangleFan 0 6
  done;
  for i = 0 to attacking_armies - 1 do
    let o = dice_order.(i) in
    let y =
      if i < min_armies then (
        GL.uniform4f basic_shader.ambient_color_location 1.0 1.0 1.0 1.0;
        Float.lerp (float_of_int (o - 1)) (float_of_int (i - 1)) dice_t_io *. -0.3
      ) else (
        GL.uniform4f basic_shader.ambient_color_location 1.0 1.0 1.0 (1.0 -. dice_t_i_f);
        Float.lerp (float_of_int (o - 1)) (float_of_int (o - 1) +. 0.5) dice_t_i_f *. -0.3
      )
    in
    GL.uniform2f basic_shader.vertex_coords_offset_location (-0.3) y;
    GL.uniform2f basic_shader.texture_coords_offset_location (float_of_int dice_points.(i) *. 0.125) 0.0;
    Render.draw_basic basic_shader battle_resolution_data.texture battle_resolution_data.vertex_buffer GL.TriangleFan 16 4
  done;
  for i = 0 to defending_armies - 1 do
    let o = dice_order.(i + 3) in
    let y =
      if i < min_armies then (
        GL.uniform4f basic_shader.ambient_color_location 1.0 1.0 1.0 1.0;
        Float.lerp (float_of_int (o - 1)) (float_of_int (i - 1)) dice_t_io *. -0.3
      ) else (
        GL.uniform4f basic_shader.ambient_color_location 1.0 1.0 1.0 (1.0 -. dice_t_i_f);
        Float.lerp (float_of_int (o - 1)) (float_of_int (o - 1) +. 0.5) dice_t_i_f *. -0.3
      )
    in
    GL.uniform2f basic_shader.vertex_coords_offset_location 0.3 y;
    GL.uniform2f basic_shader.texture_coords_offset_location (float_of_int dice_points.(i + 3) *. 0.125) 0.25;
    Render.draw_basic basic_shader battle_resolution_data.texture battle_resolution_data.vertex_buffer GL.TriangleFan 16 4
  done
