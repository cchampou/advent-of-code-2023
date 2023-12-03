(* position are in the form x * y, ie. col * line *)

let () =
  let lines = Advent.read_file "inputs/day3" in
  let pparts_unwrapped =
    lines
    |> List.map (fun x ->
           print_endline x;
           x)
    |> List.mapi (fun i -> Parser.find_potential_part_in_line [] (0, i))
    |> List.flatten
    |> List.map Transformer.potential_part_unwrap
    |> List.map Logger.log_ppart_unwrapped
  in
  pparts_unwrapped |> ignore;
  let symbols =
    lines
    |> List.mapi (fun i -> Symbols.find_symbols [] (0, i))
    |> List.flatten |> List.map Logger.log_symbol
  in
  Sum.count pparts_unwrapped symbols 0 0 |> Printf.printf "Result %d";
  ()
