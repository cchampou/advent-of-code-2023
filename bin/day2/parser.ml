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
  String.split_on_char ';' game_str |> List.map String.trim

let convert_to_pair (set : string) : int * Types.color =
  String.split_on_char ' ' set |> List.map String.trim |> fun y ->
  let number = int_of_string (List.nth y 0) in
  match List.nth y 1 with
  | "red" -> (number, Types.Red)
  | "green" -> (number, Types.Green)
  | "blue" -> (number, Types.Blue)
  | _ -> failwith "Unkown color"

let split_set (set : string) : (int * Types.color) list =
  String.split_on_char ',' set
  |> List.map String.trim |> List.map convert_to_pair

let is_valid instructions (x, y) : unit =
  Printf.printf "Cheking if %d is valid for color: " x;
  Logger.log_color y;
  print_endline "";
  let element = List.find (fun s -> snd s = y) instructions in
  if fst element < x then failwith "Not valid" else ()

let check_set instructions : (int * Types.color) list -> unit =
  List.iter (is_valid instructions)

let max_value_unit (value, color) current_max =
  if snd current_max == color && value > fst current_max then (value, color)
  else current_max

let max_value (current_max_table : (int * Types.color) list)
    (element : int * Types.color) =
  List.map (max_value_unit element) current_max_table

let rec get_max acc sets =
  match sets with
  | [] -> acc
  | head :: tail -> get_max (max_value acc head) tail

let parse_game_line line : (int * Types.color) list =
  get_game_string line |> get_game_str_set |> List.map split_set
  |> List.map Logger.log_set |> List.flatten
  |> get_max [ (0, Types.Red); (0, Types.Blue); (0, Types.Green) ]
(* |> List.iter (check_set instructions) *)
