let () =
  Advent.read_file "inputs/day4"
  |> List.map Parser.remove_line_name
  |> List.map Parser.split_win_played
  |> List.map Parser.convert_line_to_int_lists
  |> List.map Score.add_score_occurence
  |> Score.distribute_occurences |> Score.count_occurences 0
  |> Printf.printf "%d"
