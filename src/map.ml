type territory = {
    name: string;
    id: string;
    adjacent: string list;
    shape: Vec2.t array;
    color: Utils.color;
  }

type continent = {
    name: string;
    reinforcement: int;
    territories: string list;
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
  let shape_offset = ref 0 in
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
  let read_background _attrs =
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
           | [] | [ _ ] -> Array.of_list (List.rev acc)
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
    let color = attr_assoc_opt "color" attrs |> Option.value ~default:"#ffffff" |> Utils.color_of_hex in
    let shape =
      let tmp_shape = aux default_shape in
      if tmp_shape == default_shape then (
        let dx = float_of_int (!shape_offset mod 5 - 2) *. 0.25 in
        let dy = float_of_int (2 - !shape_offset / 5) *. 0.25 in
        incr shape_offset;
        Array.map Vec2.(add { x = dx; y = dy }) default_shape
      ) else tmp_shape
    in
    { name; id; adjacent; shape; color }
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
  let territories = Array.of_list (List.rev !territories) in
  let continents = Array.of_list (List.rev !continents) in
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
      let attrs =
        [ ("", "name"), t.name;
          ("", "id"), t.id;
          ("", "adjacent"), String.concat "," t.adjacent;
          ("", "color"), Utils.hex_of_color t.color ]
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

let find_territory_at_coord map coord =
  let for_all_successive_pairs_cycle f a =
    let len = Array.length a in
    let rec aux = function
      | i when i + 1 = len -> f a.(i) a.(0)
      | i when f a.(i) a.(i + 1) -> aux (i + 1)
      | _ -> false
    in
    len < 2 || aux 0
  in
  let len = Array.length map.territories in
  let rec loop = function
    | i when i = len -> None
    | i when for_all_successive_pairs_cycle (fun v1 v2 -> Vec2.side v1 v2 coord <= 0.0) map.territories.(i).shape -> Some map.territories.(i)
    | i -> loop (i + 1)
  in
  loop 0
