let potential_part_unwrap (ppart : Types.potential_part) :
    Types.unwrapped_potential_part =
  let x, y = fst ppart in
  let len = Advent.length_of_int (snd ppart) in
  ((fst ppart, (x + len - 1, y)), snd ppart)
