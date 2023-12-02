let rec list_digits line i digits =
  let len = String.length line in
  if i == len then List.rev digits
  else
    let char_at_i = line.[i] in
    let next_i = i + 1 in
    if Utils.is_numeric char_at_i then
      let new_digits = char_at_i :: digits in
      list_digits line next_i new_digits
    else list_digits line next_i digits

let get_tuple numbers =
  let first = Utils.get_first numbers in
  let last = Utils.get_last numbers in
  let first_digit = int_of_char first - int_of_char '0' in
  let last_digit = int_of_char last - int_of_char '0' in
  (first_digit, last_digit)

let rec process_line lines index total =
  let len = List.length lines in
  if index = len then total
  else
    let line = List.nth lines index in
    let numbers = list_digits line 0 [] in
    let first_digit, last_digit = get_tuple numbers in
    let current_total = last_digit + (10 * first_digit) in
    Printf.printf "%d" current_total;
    print_newline ();
    process_line lines (index + 1) (current_total + total)

let () =
  let filename = "input" in
  let lines = Reader.read_file filename in
  let total = process_line lines 0 0 in
  Printf.printf "Result: %d\n" total
