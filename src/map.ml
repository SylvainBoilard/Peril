open Utils

type territory = {
    name: string;
    id: string;
    adjacent: string list;
    shape: vec2 array;
    color: color;
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
    { x = 0.1; y = 0.1 };
    { x = -0.1; y = 0.1 };
    { x = -0.1; y = -0.1 };
    { x = 0.1; y = -0.1 }
  |]

let offset_shape dx dy shape =
  Array.map (fun { x; y } -> { x = x +. dx; y = y +. dy }) shape

let load_from_xml_file filename =
  let open Xmlm in
  let shape_offset = ref 0 in
  let in_map = open_in filename in
  let xml_map = make_input ~strip:true (`Channel in_map) in
  let background = ref "" in
  let territories = Hashtbl.create 64 in
  let continents = Hashtbl.create 8 in
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
              pack ({ x; y } :: acc) tl
         in
         aux (pack [] Str.(split (regexp "[, ]+") data))
      | `Dtd _ -> aux acc
      | `El_start _ -> drop (); aux acc
      | `El_end -> acc
    in
    let name = attr_assoc "name" attrs in
    let id = attr_assoc "id" attrs in
    let adjacent = attr_assoc "adjacent" attrs |> String.split_on_char ',' in
    let color = attr_assoc_opt "color" attrs |> Option.value ~default:"#ffffff" |> color_of_hex in
    let dx = float_of_int (!shape_offset mod 3 - 1) *. 0.25 in
    let dy = float_of_int (!shape_offset / 3 - 1) *. 0.25 in
    incr shape_offset;
    let shape = aux (offset_shape dx dy default_shape) in
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
       Hashtbl.add territories territory.id territory;
       peril_scope ()
    | `El_start ((_, "continent"), attrs) ->
       let continent = read_continent attrs in
       Hashtbl.add continents continent.name continent;
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
  let territories = Array.of_seq (Hashtbl.to_seq_values territories) in
  let continents = Array.of_seq (Hashtbl.to_seq_values continents) in
  { background; territories; continents }
