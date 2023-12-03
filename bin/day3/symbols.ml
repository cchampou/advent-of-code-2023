let rec find_symbols symbols pos line =
  let len = String.length line in
  match len with
  | 0 -> symbols
  | _ ->
      let character = line.[0] in
      let next_pos = (fst pos + 1, snd pos) in
      let rest_line = String.sub line 1 (len - 1) in
      if character = '*' then
        find_symbols (symbols @ [ pos ]) next_pos rest_line
      else find_symbols symbols next_pos rest_line
