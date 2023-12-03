let log_ppart (ppart : Types.potential_part) : unit =
  let x, y = fst ppart in
  Printf.printf "Part %d at (%d,%d)\n" (snd ppart) x y

let log_ppart_unwrapped (ppart : Types.unwrapped_potential_part) :
    Types.unwrapped_potential_part =
  let x, y = fst (fst ppart) in
  let a, b = snd (fst ppart) in
  Printf.printf "Part %d at (%d,%d) - (%d,%d)\n" (snd ppart) x y a b;
  ppart

let log_symbol (x, y) =
  Printf.printf "Symbol (%d,%d)\n" x y;
  (x, y)
