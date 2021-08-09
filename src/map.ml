open Utils

type territory = {
    name: string;
    id: string;
    adjacent: string list;
    shape: Vec2.t array;
    center: Vec2.t;
  }

type continent = {
    name: string;
    reinforcement: int;
    territories: string list;
  }

type t = {
    background: string;
    territories: territory array;
    territories_by_id: int array;
    continents: continent array;
  }

let find_territory_offset map id =
  Array.find_sorted (fun i ->
      String.compare id map.territories.(i).id
    ) map.territories_by_id

let find_territory_offset_opt map id =
  Array.find_sorted_opt (fun i ->
      String.compare id map.territories.(i).id
    ) map.territories_by_id

let find_territory map id =
  map.territories.(find_territory_offset map id)

let find_territory_opt map id =
  Option.map (Array.get map.territories) (find_territory_offset_opt map id)

let rec attr_assoc name = function
  | [] -> raise Not_found
  | ((_, hd_name), hd_value) :: _ when hd_name = name -> hd_value
  | _ :: tl -> attr_assoc name tl

let rec attr_assoc_opt name = function
  | [] -> None
  | ((_, hd_name), hd_value) :: _ when hd_name = name -> Some hd_value
  | _ :: tl -> attr_assoc_opt name tl

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
    | `El_end -> ()
    | `El_start _ -> drop (); drop ()
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
  let read_territory attrs =
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
    let name = attr_assoc "name" attrs in
    let id = attr_assoc "id" attrs in
    let adjacent = attr_assoc "adjacent" attrs |> String.split_on_char ',' in
    let shape =
      let tmp_shape = aux default_shape in
      if tmp_shape == default_shape then (
        let dx = float_of_int (!default_shape_offset mod 5 - 2) *. 0.25 in
        let dy = float_of_int (2 - !default_shape_offset / 5) *. 0.25 in
        incr default_shape_offset;
        Array.map Vec2.(add { x = dx; y = dy }) default_shape
      ) else tmp_shape
    in
    let center = compute_shape_barycenter shape in
    { name; id; adjacent; shape; center }
  in
  let read_continent attrs =
    drop ();
    let name = attr_assoc "name" attrs in
    let reinforcement = attr_assoc "reinforcement" attrs |> int_of_string in
    let territories = attr_assoc "territories" attrs |> String.split_on_char ',' in
    { name; reinforcement; territories }
  in
  let rec peril_scope () = match input xml_map with
    | `Data _ | `Dtd _ -> peril_scope ()
    | `El_end -> ()
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
  in
  let rec global_scope () = match input xml_map with
    | `Data _ | `Dtd _ -> global_scope ()
    | `El_end -> assert false
    | `El_start ((_, "peril"), _) -> peril_scope ()
    | `El_start _ -> drop (); global_scope ()
  in
  global_scope ();
  close_in in_map;
  let background = !background in
  let territories = Array.of_rev_list !territories in
  let territories_by_id = Array.(init (length territories) Fun.id) in
  Array.sort (fun i1 i2 ->
      String.compare territories.(i1).id territories.(i2).id
    ) territories_by_id;
  let continents = Array.of_rev_list !continents in
  { background; territories; territories_by_id; continents }

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
      let attrs =
        [ ("", "name"), t.name;
          ("", "id"), t.id;
          ("", "adjacent"), String.concat "," t.adjacent ]
      in
      output xml_map (`El_start (("", "territory"), attrs));
      let data =
        Array.map (fun Vec2.{x;y} -> Printf.sprintf "%g, %g" x y) t.shape
        |> Array.to_list |> String.concat ", "
      in
      output xml_map (`Data data);
      output xml_map (`El_end)
    ) map.territories;
  Array.iter (fun (c : continent) ->
      let attrs =
        [ ("", "name"), c.name;
          ("", "reinforcement"), string_of_int c.reinforcement;
          ("", "territories"), String.concat "," c.territories ]
      in
      output xml_map (`El_start (("", "continent"), attrs));
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
  Array.fold_left (fun ids t ->
      if List.mem t.id ids
      then (Printf.eprintf "%s reuses id %s.\n%!" t.name t.id; ids)
      else t.id :: ids
    ) [] map.territories |> ignore;
  Array.iter (fun t ->
      if not (shape_is_convex t.shape) then
        Printf.eprintf "%s's shape is not convex.\n%!" t.name;
      if not (Array.exists (fun (c : continent) -> List.mem t.id c.territories) map.continents) then
        Printf.eprintf "%s is not part of a continent.\n%!" t.name;
      if t.adjacent = [] then
        Printf.eprintf "%s is not adjacent to any territory.\n%!" t.name
      else
        List.iter (fun id' ->
            match find_territory_opt map id' with
            | _ when t.id = id' -> Printf.eprintf "%s is adjacent to itself.\n%!" t.name
            | Some t' when not (List.exists ((=) t.id) t'.adjacent) ->
               Printf.eprintf "%s is adjacent to %s but not the other way around.\n%!" t.name t'.name
            | None -> Printf.eprintf "%s is adjacent to inexistant territory with id %s.\n%!" t.name id'
            | _ -> ()
          ) t.adjacent
    ) map.territories;
  Array.iter (fun c ->
      List.iter (fun id ->
          match find_territory_opt map id with
          | Some _ -> ()
          | None -> Printf.eprintf "%s has inexistant territory with id %s.\n%!" c.name id
        ) c.territories
    ) map.continents

let find_territory_at_coords map coords =
  Array.find_opt (fun t ->
      Array.for_all_successive_pairs_loop (fun v1 v2 -> Vec2.side v1 v2 coords <= 0.0) t.shape
    ) map.territories

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
