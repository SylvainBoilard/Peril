open Utils

type rgba = { r: float; g: float; b: float; a: float }
type hsla = { h: float; s: float; l: float; a: float }

type name =
  | Black
  | White
  | Red
  | Green
  | Blue
  | Orange
  | Yellow
  | Purple

let string_of_name = function
  | Black -> "black"
  | White -> "white"
  | Red -> "red"
  | Green -> "green"
  | Blue -> "blue"
  | Orange -> "orange"
  | Yellow -> "yellow"
  | Purple -> "purple"

let rgba_of_name = function
  | Black -> { r = 0.0; g = 0.0; b = 0.0; a = 1.0 }
  | White -> { r = 1.0; g = 1.0; b = 1.0; a = 1.0 }
  | Red -> { r = 1.0; g = 0.0; b = 0.0; a = 1.0 }
  | Green -> { r = 0.0; g = 1.0; b = 0.0; a = 1.0 }
  | Blue -> { r = 0.0; g = 0.0; b = 1.0; a = 1.0 }
  | Orange -> { r = 1.0; g = 0.5; b = 0.0; a = 1.0 }
  | Yellow -> { r = 1.0; g = 1.0; b = 0.0; a = 1.0 }
  | Purple -> { r = 1.0; g = 0.0; b = 1.0; a = 1.0 }

let hsla_of_name = function
  | Black -> { h = 0.0; s = 0.0; l = 0.0; a = 1.0 }
  | White -> { h = 0.0; s = 0.0; l = 1.0; a = 1.0 }
  | Red -> { h = 0.0; s = 1.0; l = 0.5; a = 1.0 }
  | Green -> { h = 120.0; s = 1.0; l = 0.5; a = 1.0 }
  | Blue -> { h = 240.0; s = 1.0; l = 0.5; a = 1.0 }
  | Orange -> { h = 30.0; s = 1.0; l = 0.5; a = 1.0 }
  | Yellow -> { h = 60.0; s = 1.0; l = 0.5; a = 1.0 }
  | Purple -> { h = 300.0; s = 1.0; l = 0.5; a = 1.0 }

let rgba_of_hsla c =
  let r_base = Float.(clamp 0.0 1.0 (abs ((c.h -. 180.0) /. 60.0) -. 1.0)) in
  let g_base = Float.(clamp 0.0 1.0 (2.0 -. abs ((c.h -. 120.0) /. -60.0))) in
  let b_base = Float.(clamp 0.0 1.0 (2.0 -. abs ((c.h -. 240.0) /. -60.0))) in
  let r_sat = 0.5 +. (r_base -. 0.5) *. c.s in
  let g_sat = 0.5 +. (g_base -. 0.5) *. c.s in
  let b_sat = 0.5 +. (b_base -. 0.5) *. c.s in
  let r, g, b =
    if c.l <= 0.5 then
      let l = c.l *. 2.0 in
      r_sat *. l, g_sat *. l, b_sat *. l
    else
      let l = c.l *. 2.0 -. 1.0 in
      r_sat +. (1.0 -. r_sat) *. l, g_sat +. (1.0 -. g_sat) *. l, b_sat +. (1.0 -. b_sat) *. l
  in
  { r; g; b; a = c.a }

let of_hex str =
  Scanf.sscanf str "#%2x%2x%2x" (fun r g b ->
      { r = float_of_int r /. 255.0;
        g = float_of_int g /. 255.0;
        b = float_of_int b /. 255.0;
        a = 1.0 })

let to_hex color =
  Printf.sprintf "#%02x%02x%02x"
    (truncate (color.r *. 255.0))
    (truncate (color.g *. 255.0))
    (truncate (color.b *. 255.0))
