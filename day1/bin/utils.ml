let is_numeric c =
  let ascii_code = Char.code c in
  ascii_code >= Char.code '0' && ascii_code <= Char.code '9'

let get_first list_of_elem = List.nth list_of_elem 0

let get_last list_of_elem =
  let len = List.length list_of_elem in
  List.nth list_of_elem (len - 1)

let char_to_int character = int_of_char character - int_of_char '0'
