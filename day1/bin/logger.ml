let log_action action complete =
  if complete then print_endline (action ^ " ✅")
  else print_endline (action ^ " ⚙️")
