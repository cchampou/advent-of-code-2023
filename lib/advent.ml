let log_action action complete =
  if complete then print_endline (action ^ " ✅")
  else print_endline (action ^ " ⚙️")

let read_file filename : string list =
  log_action "reading" false;
  try
    let channel = open_in filename in
    let rec read_lines acc =
      try
        let line = input_line channel in
        read_lines (line :: acc)
      with End_of_file ->
        close_in channel;
        List.rev acc
    in
    let lines = read_lines [] in
    log_action "reading" true;
    lines
  with Sys_error msg -> failwith ("Error: " ^ msg)

let is_numeric c =
  let ascii_code = Char.code c in
  ascii_code >= Char.code '0' && ascii_code <= Char.code '9'

let char_to_int character = int_of_char character - int_of_char '0'
let length_of_int n = string_of_int n |> String.length

let rec parse_number (line : string) (total : int) : int =
  let len = String.length line in
  match line with
  | "" -> total
  | _ ->
      if is_numeric line.[0] != true then total
      else
        parse_number
          (String.sub line 1 (len - 1))
          ((total * 10) + char_to_int line.[0])
