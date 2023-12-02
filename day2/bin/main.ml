let rec process_lines lines index total : int =
  let game_id = index + 1 in
  if index = List.length lines then total
  else
    try
      Parser.parse_game_line (List.nth lines index) Constants.get_instructions;
      Printf.printf "Game %d is possible\n" game_id;
      process_lines lines (index + 1) (total + game_id)
    with _ -> process_lines lines (index + 1) total

let () =
  let filename = "input" in
  let lines = Reader.read_file filename in
  let result = process_lines lines 0 0 in
  Printf.printf "%d" result
