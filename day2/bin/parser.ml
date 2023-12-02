(**
This function take a full line "Game 1: 3 blue, 3 red; ..."
and return the right part trimmed.
 *)
let get_game_string (line : string) : string =
  let split = String.split_on_char ':' line in
  match split with
  | _ :: game_data :: _ -> String.trim game_data
  | _ -> failwith "Incorrect input, expected: `Game n: data`"

let get_game_str_set (game_str : string) : string list =
  let split = String.split_on_char ';' game_str in
  List.map String.trim split

let convert_to_pair (set : string) : int * Types.color =
  String.split_on_char ' ' set |> List.map String.trim |> fun y ->
  let number = int_of_string (List.nth y 0) in
  match List.nth y 1 with
  | "red" -> (number, Types.Red)
  | "green" -> (number, Types.Green)
  | "blue" -> (number, Types.Blue)
  | _ -> failwith "Unkown color"

let split_set (set : string) : (int * Types.color) list =
  let split = String.split_on_char ',' set in
  List.map String.trim split |> List.map convert_to_pair

let is_valid (x, y) instructions : bool =
  Printf.printf "Cheking if %d" x;
  Logger.log_color y;
  print_endline "";
  let element = List.find (fun s -> snd s = y) instructions in
  if fst element < x then failwith "Not valid" else true

let check_set instructions set : unit =
  let len = List.length set in
  for i = 0 to len - 1 do
    let current = List.nth set i in
    ignore (is_valid current instructions)
  done;
  ()

let parse_game_line line instructions : unit =
  let game_string = get_game_string line in
  let sets = get_game_str_set game_string in
  let splitted_sets = List.map split_set sets in
  List.iter Logger.log_set splitted_sets;
  List.iter (check_set instructions) splitted_sets
