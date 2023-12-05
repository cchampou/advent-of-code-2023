let print_int_list l = List.iter (Printf.printf "|%d|") l

let print_parsed_line ((win, play), (score, occurence)) =
  print_string "((";
  print_int_list win;
  print_int_list play;
  print_endline "),(";
  print_int score;
  print_char ',';
  print_int occurence;
  print_endline ")"
