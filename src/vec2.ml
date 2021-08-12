type t = { x: float; y: float }

let zero = { x = 0.0; y = 0.0 }

let rot_dir v = { x = -.v.y; y = v.x }

let rot_indir v = { x = v.y; y = -.v.x }

let add v1 v2 = { x = v1.x +. v2.x; y = v1.y +. v2.y }

let sub v1 v2 = { x = v1.x -. v2.x; y = v1.y -. v2.y }

let scale v f = { x = v.x *. f; y = v.y *. f }

let mult v1 v2 = { x = v1.x *. v2.x; y = v1.y *. v2.y }

let div v1 v2 = { x = v1.x /. v2.x; y = v1.y /. v2.y }

let mag v = hypot v.x v.y

let sqr_mag v = v.x *. v.x +. v.y *. v.y

let normalize v =
  let d = hypot v.x v.y in
  { x = v.x /. d; y = v.y /. d }

let dot v1 v2 = v1.x *. v2.x +. v1.y *. v2.y

let inter v1 v2 v3 v4 =
  let a = v2.x -. v1.x in
  let b = v3.x -. v4.x in
  let c = v3.x -. v1.x in
  let d = v2.y -. v1.y in
  let e = v3.y -. v4.y in
  let f = v3.y -. v1.y in
  let g = a *. e -. d *. b in
  let t = (e *. c -. b *. f) /. g in
  let u = (a *. f -. d *. c) /. g in
  t, u

let side v1 v2 p = dot (sub v2 v1) (rot_dir (sub p v2))

let lerp v1 v2 t =
  { x = v1.x +. (v2.x -. v1.x) *. t;
    y = v1.y +. (v2.y -. v1.y) *. t }

let make x y = { x; y }

let of_tuple (x, y) = { x; y }
