let get_last list_of_elem =
  let len = List.length list_of_elem in
  List.nth list_of_elem (len - 1)

let char_to_int character = int_of_char character - int_of_char '0'
