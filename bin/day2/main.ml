let rec part2 lines total : int =
  match lines with
  | [] -> total
  | head :: tail ->
      let res = Parser.parse_game_line head
      |> List.fold_left (fun acc x ->
        acc * fst x
      ) 1 in
      part2 tail (total + res)

let () =
  let filename = "inputs/day2" in
  let lines = Advent.read_file filename in
  part2 lines 0
  |> Printf.printf "%d"

