let rec parse_number (line : string) (total : int) : int =
  let len = String.length line in
  match line with
  | "" ->
      Printf.printf "Reached EOL %d\n" total;
      total
  | _ ->
      if Advent.is_numeric line.[0] != true then total
      else
        parse_number
          (String.sub line 1 (len - 1))
          ((total * 10) + Advent.char_to_int line.[0])

let rec find_potential_part_in_line (pparts : Types.potential_part list)
    (pos : int * int) (line : string) : Types.potential_part list =
  let len = String.length line in
  match len with
  | 0 -> pparts
  | _ ->
      if Advent.is_numeric line.[0] then
        let total = parse_number line 0 in
        let total_length = Advent.length_of_int total in
        let rest = String.sub line total_length (len - total_length) in
        find_potential_part_in_line ((pos, total) :: pparts)
          (fst pos + total_length, snd pos)
          rest
      else
        let rest = String.sub line 1 (len - 1) in
        find_potential_part_in_line pparts (fst pos + 1, snd pos) rest
