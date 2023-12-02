let log_action action complete =
  if complete then print_endline (action ^ " ✅")
  else print_endline (action ^ " ⚙️")

let log_game_str_set game_str_set =
  let len = List.length game_str_set in
  for i = 0 to len - 1 do
    print_endline (List.nth game_str_set i)
  done

let log_color color =
  match color with
  | Types.Red -> print_string "red"
  | Types.Green -> print_string "green"
  | Types.Blue -> print_string "blue"

let log_set (set : (int * Types.color) list) : (int * Types.color) list =
  let len = List.length set in
  print_string "[";
  for i = 0 to len - 1 do
    let x, y = List.nth set i in
    print_string (string_of_int x);
    print_string "-";
    log_color y
  done;
  print_endline "]";
  set
