let check_poximity pos symbol =
  let x, y = pos in
  let xx, yy = symbol in
  let proxx = x - xx in
  let proxy = y - yy in
  (*Printf.printf
    "===\nCheking if p(%d,%d) is close to s(%d,%d)\nProximity (%d,%d)\n" x y xx
    yy proxx proxy; *)
  if Int.abs proxx <= 1 && Int.abs proxy <= 1 then (* print_endline "✅"; *)
    true
  else (* print_endline "❌"; *)
    false

let rec adjacent_product symbol pparts acc =
  match pparts with
  | [] -> acc
  | head :: tail ->
      if
        check_poximity (fst (fst head)) symbol = true
        || check_poximity (snd (fst head)) symbol = true
      then adjacent_product symbol tail (fst acc + 1, snd acc * snd head)
      else adjacent_product symbol tail acc

let rec count pparts symbols acc =
  match symbols with
  | [] -> acc
  | head :: tail ->
      let occurrence, result = adjacent_product head pparts (0, 1) in
      Printf.printf "Found %d occurrence, giving %d for s(%d,%d)\n" occurrence
        result (fst head) (snd head);
      if occurrence = 2 then count pparts tail (acc + result)
      else count pparts tail acc
