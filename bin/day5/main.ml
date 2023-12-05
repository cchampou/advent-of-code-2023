let () =
  Advent.read_file "inputs/day5"
  |> Parser.parse_root |> Logger.log_root |> Process.process_root
