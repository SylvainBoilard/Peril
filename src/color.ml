open Utils

type rgba = { r: float; g: float; b: float; a: float }
type hsla = { h: float; s: float; l: float; a: float }
type suite = {
    normal: rgba;
    darker: rgba;
    brighter: rgba;
    desaturated: rgba;
  }

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

let hsla_of_rgba c =
  let h, min_c, max_c = match c.r < c.g, c.g < c.b, c.b < c.r with
    | true, true, _ ->
       240.0 -. 60.0 *. (c.g -. c.r) /. (c.b -. c.r), c.r, c.b
    | true, false, true ->
       120.0 -. 60.0 *. (c.r -. c.b) /. (c.g -. c.b), c.b, c.g
    | true, false, false ->
       120.0 +. 60.0 *. (c.b -. c.r) /. (c.g -. c.r), c.r, c.g
    | false, false, _ ->
         0.0 +. 60.0 *. (c.g -. c.b) /. (c.r -. c.b), c.b, c.r
    | false, true, true ->
       360.0 -. 60.0 *. (c.b -. c.g) /. (c.r -. c.g), c.g, c.r
    | false, true, false ->
       240.0 +. 60.0 *. (c.r -. c.g) /. (c.b -. c.g), c.g, c.b
  in
  let lx2 = min_c +. max_c in
  let s = (max_c -. min_c) /. (1.0 -. abs_float (1.0 -. lx2)) in
  { h = if Float.is_nan h then 0.0 else h;
    s = if Float.is_nan s then 0.0 else s;
    l = lx2 /. 2.0; a = c.a }

let make_suite c =
  let normal = rgba_of_hsla c in
  let darker = rgba_of_hsla { c with l = c.l *. 0.9 } in
  let brighter = rgba_of_hsla { c with l = c.l *. 1.1 } in
  let desaturated = rgba_of_hsla { c with s = 0.0; l = 0.25 } in
  { normal; darker; brighter; desaturated }
