let is_numeric c =
  let ascii_code = Char.code c in
  ascii_code >= Char.code '0' && ascii_code <= Char.code '9'
;;


let get_first list_of_elem =
  List.nth list_of_elem 0
;;

let get_last list_of_elem =
  let len = List.length list_of_elem in
  List.nth list_of_elem (len - 1)

let rec list_digits line i digits =
  let len = String.length line in
  if i == len then
    List.rev digits
  else
    let char_at_i = line.[i] in
    let next_i = i + 1 in
    if is_numeric char_at_i then
      let new_digits = char_at_i :: digits in
      list_digits line next_i new_digits
    else
      list_digits line next_i digits
;;

let rec process_line lines index total =
  let len = List.length lines in
  if index = len then
    total
  else
    let line = List.nth lines index in
    let numbers = list_digits line 0 [] in
    let first = get_first numbers in
    let last = get_last numbers in
    let first_digit = int_of_char first - int_of_char '0' in
    let last_digit = int_of_char last - int_of_char '0' in
    let current_total = last_digit + 10 * first_digit in
    Printf.printf "%d" current_total;
    print_newline ();
    process_line lines (index + 1) (current_total + total)
;;

let () =
  let filename = "input" in
  let lines = Reader.read_file filename in
  let total = process_line lines 0 0 in
  Printf.printf "Result: %d\n" total;
