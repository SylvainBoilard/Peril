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
