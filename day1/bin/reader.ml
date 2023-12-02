let read_file filename =
  Logger.log_action "reading" false;
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
    Logger.log_action "reading" true;
    lines
  with Sys_error msg -> failwith ("Error: " ^ msg)
