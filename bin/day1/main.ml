let rec check_letters_digit str numbers index : int =
  let len = List.length numbers in
  if index = len then failwith "Not found"
  else
    let tuple = List.nth numbers index in
    let word = fst tuple in
    if String.starts_with ~prefix:word str then snd tuple
    else check_letters_digit str numbers (index + 1)

let rec list_digits line i digits =
  let len = String.length line in
  if i == len then List.rev digits
  else
    let char_at_i = line.[i] in
    let next_i = i + 1 in
    if Utils.is_numeric char_at_i then
      let new_digits = char_at_i :: digits in
      list_digits line next_i new_digits
    else
      try
        let substr = String.sub line i (len - i) in
        let numbers = Constants.get_numbers in
        let number = check_letters_digit substr numbers 0 in
        let new_digits = char_of_int (number + int_of_char '0') :: digits in
        list_digits line next_i new_digits
      with _ -> list_digits line next_i digits

let get_tuple numbers =
  match numbers with
  | h :: _ -> (Utils.char_to_int h, Utils.char_to_int (Utils.get_last numbers))
  | _ -> failwith "Incorrect pair"

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
  let filename = "inputs/day1" in
  let lines = Advent.read_file filename in
  let total = process_line lines 0 0 in
  Printf.printf "Result: %d\n" total
