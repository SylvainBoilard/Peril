type t = { r: float; g: float; b: float; a: float }

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

let of_name = function
  | Black -> { r = 0.0; g = 0.0; b = 0.0; a = 1.0 }
  | White -> { r = 1.0; g = 1.0; b = 1.0; a = 1.0 }
  | Red -> { r = 1.0; g = 0.0; b = 0.0; a = 1.0 }
  | Green -> { r = 0.0; g = 1.0; b = 0.0; a = 1.0 }
  | Blue -> { r = 0.0; g = 0.0; b = 1.0; a = 1.0 }
  | Orange -> { r = 1.0; g = 0.5; b = 0.0; a = 1.0 }
  | Yellow -> { r = 1.0; g = 1.0; b = 0.0; a = 1.0 }
  | Purple -> { r = 1.0; g = 0.0; b = 1.0; a = 1.0 }

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
