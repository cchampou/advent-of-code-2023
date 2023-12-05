let remove_line_name line =
  let parts = String.split_on_char ':' line in
  match parts with [ _; b ] -> String.trim b | _ -> failwith "Invalid line"

let split_win_played line : string * string =
  let split = String.split_on_char '|' line in
  match split with
  | [ a; b ] -> (String.trim a, String.trim b)
  | _ -> failwith "Line is not separated by a |"

let rec convert_string_to_int_list (l : int list) line =
  let len = String.length line in
  if len = 0 then l
  else
    match line.[0] with
    | ' ' -> convert_string_to_int_list l (String.sub line 1 (len - 1))
    | _ ->
        let number = Advent.parse_number line 0 in
        let nb_len = Advent.length_of_int number in
        let new_l = l @ [ number ] in
        convert_string_to_int_list new_l (String.sub line nb_len (len - nb_len))

let convert_line_to_int_lists (win, play) : int list * int list =
  (convert_string_to_int_list [] win, convert_string_to_int_list [] play)
