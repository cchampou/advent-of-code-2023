let rec print_int_list (l : int list) =
  match l with
  | [] -> ()
  | h :: t ->
      print_int h;
      if List.length t > 0 then print_char ',';
      print_int_list t

let log_transformation_unit (t : Types.transformation) =
  Printf.printf "destination: %d source: %d range: %d" t.destination t.source
    t.range

let rec log_transformations (t : Types.transformation list) =
  match t with
  | [] -> ()
  | h :: t ->
      log_transformation_unit h;
      if List.length t > 0 then print_char '\n';
      log_transformations t

let rec log_transformation_steps (l : Types.transformation list list) =
  match l with
  | [] -> ()
  | h :: t ->
      log_transformations h;
      if List.length t > 0 then print_endline "\n===";
      log_transformation_steps t

let log_root (root : Types.root) =
  let action_name = "logging root" in
  Advent.log_action action_name false;
  print_string "seeds: ";
  print_int_list root.seeds;
  print_endline "\ntransformations:";
  log_transformation_steps root.transformations;
  print_char '\n';
  Advent.log_action action_name true;
  root
