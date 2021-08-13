open Utils

type territory = {
    name: string;
    adjacent: int array;
    shape: Vec2.t array;
    center: Vec2.t;
  }

type continent = {
    name: string;
    reinforcement: int;
    territories: int array;
  }

type t = {
    background: string;
    territories: territory array;
    continents: continent array;
  }

let rec attr_assoc name = function
  | [] -> raise Not_found
  | ((_, hd_name), hd_value) :: _ when hd_name = name -> hd_value
  | _ :: tl -> attr_assoc name tl

let default_shape = [|
    Vec2.{ x = 0.1; y = 0.1 };
    { x = -0.1; y = 0.1 };
    { x = -0.1; y = -0.1 };
    { x = 0.1; y = -0.1 }
  |]

let load_from_xml_file filename =
  let open Xmlm in
  let default_shape_offset = ref 0 in
  let in_map = open_in filename in
  let xml_map = make_input ~strip:true (`Channel in_map) in
  let background = ref "" in
  let territories = ref [] in
  let continents = ref [] in
  let rec drop () = match input xml_map with
    | `Data _ | `Dtd _ -> drop ()
    | `El_start _ -> drop (); drop ()
    | `El_end -> ()
  in
  let read_background _(*attrs*) =
    let rec aux acc = match input xml_map with
      | `Data filename -> aux filename
      | `Dtd _ -> aux acc
      | `El_start _ -> drop (); aux acc
      | `El_end -> acc
    in
    aux ""
  in
  let read_adjacent _(*attrs*) =
    let rec aux acc = match input xml_map with
      | `Data data -> aux Str.(split (regexp " *, *") data)
      | `Dtd _ -> aux acc
      | `El_start _ -> drop (); aux acc
      | `El_end -> acc
    in
    aux []
  in
  let read_shape _(*attrs*) =
    let rec aux acc = match input xml_map with
      | `Data data ->
         let rec pack acc = function
           | [] | [ _ ] -> Array.of_rev_list acc
           | x :: y :: tl ->
              let x, y = float_of_string x, float_of_string y in
              pack (Vec2.{ x; y } :: acc) tl
         in
         aux (pack [] Str.(split (regexp "[, ]+") data))
      | `Dtd _ -> aux acc
      | `El_start _ -> drop (); aux acc
      | `El_end -> acc
    in
    aux default_shape
  in
  let read_territory attrs =
    let rec aux adjacent shape = match input xml_map with
      | `Data _ | `Dtd _ -> aux adjacent shape
      | `El_start ((_, "adjacent"), attrs) -> aux (read_adjacent attrs) shape
      | `El_start ((_, "shape"), attrs) -> aux adjacent (read_shape attrs)
      | `El_start _ -> drop (); aux adjacent shape
      | `El_end -> adjacent, shape
    in
    let name = attr_assoc "name" attrs in
    let adjacent, tmp_shape = aux [] default_shape in
    let shape =
      if tmp_shape == default_shape then (
        let dx = float_of_int (!default_shape_offset mod 5 - 2) *. 0.25 in
        let dy = float_of_int (2 - !default_shape_offset / 5) *. 0.25 in
        incr default_shape_offset;
        Array.map Vec2.(add { x = dx; y = dy }) default_shape
      ) else tmp_shape
    in
    name, adjacent, shape
  in
  let read_continent attrs =
    let rec aux acc = match input xml_map with
      | `Data data -> aux Str.(split (regexp " *, *") data)
      | `Dtd _ -> aux acc
      | `El_start _ -> drop (); aux acc
      | `El_end -> acc
    in
    let name = attr_assoc "name" attrs in
    let reinforcement = attr_assoc "reinforcement" attrs |> int_of_string in
    let territories = aux [] in
    name, reinforcement, territories
  in
  let rec peril_scope () = match input xml_map with
    | `Data _ | `Dtd _ -> peril_scope ()
    | `El_start ((_, "background"), attrs) ->
       background := read_background attrs;
       peril_scope ()
    | `El_start ((_, "territory"), attrs) ->
       let territory = read_territory attrs in
       territories := territory :: !territories;
       peril_scope ()
    | `El_start ((_, "continent"), attrs) ->
       let continent = read_continent attrs in
       continents := continent :: !continents;
       peril_scope ()
    | `El_start _ -> drop (); peril_scope ()
    | `El_end -> ()
  in
  let rec global_scope () = match input xml_map with
    | `Data _ | `Dtd _ -> global_scope ()
    | `El_start ((_, "peril"), _) -> peril_scope ()
    | `El_start _ -> drop (); global_scope ()
    | `El_end -> assert false
  in
  global_scope ();
  close_in in_map;
  let open Array in
  let background = !background in
  let territories = of_rev_list !territories in
  let continents =
    map (fun (name, reinforcement, cont_territories) ->
        let territories =
          map (fun t_name ->
              find_offset (fun (name', _, _) -> t_name = name') territories
            ) (of_list cont_territories)
        in
        { name; reinforcement; territories }
      ) (of_rev_list !continents)
  in
  let territories =
    map (fun (name, adjacent, shape) ->
        let adjacent =
          map (fun adj_name ->
              find_offset (fun (name', _, _) -> adj_name = name') territories
            ) (of_list adjacent)
        in
        let center = compute_shape_barycenter shape in
        { name; adjacent; shape; center }
      ) territories
  in
  { background; territories; continents }

let save_to_xml_file map filename =
  let open Xmlm in
  let out_map = open_out filename in
  let xml_map = make_output ~nl:true ~indent:(Some 2) (`Channel out_map) in
  output xml_map (`Dtd None);
  output xml_map (`El_start (("", "peril"), []));
  output xml_map (`El_start (("", "background"), []));
  output xml_map (`Data map.background);
  output xml_map (`El_end);
  Array.iter (fun (t : territory) ->
      let attrs = [ ("", "name"), t.name ] in
      output xml_map (`El_start (("", "territory"), attrs));
      output xml_map (`El_start (("", "adjacent"), []));
      let adjacent_data =
        Array.map (fun i -> map.territories.(i).name) t.adjacent
        |> Array.to_list |> String.concat ", "
      in
      output xml_map (`Data adjacent_data);
      output xml_map (`El_end);
      output xml_map (`El_start (("", "shape"), []));
      let shape_data =
        Array.map (fun Vec2.{x;y} -> Printf.sprintf "%g, %g" x y) t.shape
        |> Array.to_list |> String.concat ", "
      in
      output xml_map (`Data shape_data);
      output xml_map (`El_end);
      output xml_map (`El_end)
    ) map.territories;
  Array.iter (fun (c : continent) ->
      let attrs =
        [ ("", "name"), c.name;
          ("", "reinforcement"), string_of_int c.reinforcement ]
      in
      output xml_map (`El_start (("", "continent"), attrs));
      let data = String.concat ", " (Array.to_list (Array.map (fun i -> map.territories.(i).name) c.territories)) in
      output xml_map (`Data data);
      output xml_map (`El_end)
    ) map.continents;
  output xml_map (`El_end);
  close_out out_map

let shape_is_convex shape =
  let aux v1 v2 =
    Array.for_all (fun v -> v == v1 || v == v2 || Vec2.side v1 v2 v <= 0.0) shape
  in
  Array.length shape <= 2 || Array.for_all_successive_pairs_loop aux shape

let validate map =
  Array.iteri (fun i t ->
      if not (shape_is_convex t.shape) then
        Printf.eprintf "%s's shape is not convex.\n%!" t.name;
      if not (Array.exists (fun (c : continent) -> Array.mem i c.territories) map.continents) then
        Printf.eprintf "%s is not part of a continent.\n%!" t.name;
      if t.adjacent = [||] then
        Printf.eprintf "%s is not adjacent to any territory.\n%!" t.name
      else
        Array.iter (fun j ->
            if i = j then
              Printf.eprintf "%s is adjacent to itself.\n%!" t.name
            else if not (Array.exists ((=) i) map.territories.(j).adjacent) then
              Printf.eprintf "%s is adjacent to %s but not the other way around.\n%!" t.name map.territories.(j).name
          ) t.adjacent
    ) map.territories

let find_territory_at_coords map coords =
  try
    Array.find_offset (fun t ->
        Array.for_all_successive_pairs_loop (fun v1 v2 -> Vec2.side v1 v2 coords <= 0.0) t.shape
      ) map.territories
  with Not_found -> -1

type shape_poi = Corner of int | Edge of int * int | NoPOI

let find_poi_of_shape_at_coords shape coords =
  let len = Array.length shape in
  let rec aux_edges = function
    | i when i + 1 = len ->
       if Vec2.(sqr_mag (sub (lerp shape.(i) shape.(0) 0.5) coords)) <= 0.0005
       then Edge (i, 0)
       else NoPOI
    | i ->
       if Vec2.(sqr_mag (sub (lerp shape.(i) shape.(i + 1) 0.5) coords)) <= 0.0005
       then Edge (i, i + 1)
       else aux_edges (i + 1)
  in
  let rec aux_corners = function
    | i when i = len -> aux_edges 0
    | i when Vec2.(sqr_mag (sub shape.(i) coords)) <= 0.0005 -> Corner i
    | i -> aux_corners (i + 1)
  in
  aux_corners 0
