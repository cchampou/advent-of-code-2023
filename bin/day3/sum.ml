let check_poximity pos symbol =
  let x, y = pos in
  let xx, yy = symbol in
  let proxx = x - xx in
  let proxy = y - yy in
  Printf.printf
    "===\nCheking if p(%d,%d) is close to s(%d,%d)\nProximity (%d,%d)\n" x y xx
    yy proxx proxy;
  if Int.abs proxx <= 1 && Int.abs proxy <= 1 then (
    print_endline "✅";
    true)
  else (
    print_endline "❌";
    false)

let rec count pparts symbols sindex acc =
  match pparts with
  | [] -> acc
  | head :: tail ->
      let ((x, y), (xx, yy)), value = head in
      let current_symbol = List.nth symbols sindex in
      Printf.printf "Checking %d p(%d,%d) - p(%d,%d) against s(%d,%d)\n" value x y xx yy
        (fst current_symbol) (snd current_symbol);
      if
        check_poximity (x, y) current_symbol = true
        || check_poximity (xx, yy) current_symbol = true
      then (
        Printf.printf "Part %d is ok, adding\n" value;
        count tail symbols 0 (acc + value))
      else if sindex = List.length symbols - 1 then(print_endline "Not more symbol"; count tail symbols 0 acc)
      else count pparts symbols (sindex + 1) acc
