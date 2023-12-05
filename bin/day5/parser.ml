let parse_seeds line : int list =
  let split = String.split_on_char ':' line in
  String.split_on_char ' ' (String.trim (List.nth split 1))
  |> List.map (fun x -> Advent.parse_number x 0)

let parse_transformation_unit line : Types.transformation =
  let split = String.split_on_char ' ' line in
  Printf.printf "Parsing transformation unit %s\n" line;
  match split with
  | [ destination; source; range ] ->
      {
        destination = Advent.parse_number destination 0;
        source = Advent.parse_number source 0;
        range = Advent.parse_number range 0;
      }
  | _ -> failwith "Invalid transformation unit"

let rec parse_transformations lines : Types.transformation list =
  match lines with
  | h :: t ->
      if (String.length h > 0 && Advent.is_numeric h.[0] = false) || h = "" then
        []
      else parse_transformation_unit h :: parse_transformations t
  | [] -> []

let rec parse_transformations_steps lines : Types.transformation list list =
  match lines with
  | [] -> []
  | h :: t ->
      if
        String.length h = 0
        || String.starts_with ~prefix:"seeds" h
        || Advent.is_numeric h.[0] = false
      then (
        Printf.printf "skipping line '%s'\n" h;
        parse_transformations_steps t)
      else
        let transformations_for_step = parse_transformations lines in
        let rest : string list =
          List.filteri (fun i _ -> i >= List.length transformations_for_step) t
        in
        transformations_for_step :: parse_transformations_steps rest

let parse_root lines : Types.root =
  let action_name = "parsing root" in
  Advent.log_action action_name false;
  let root : Types.root =
    {
      seeds = parse_seeds (List.nth lines 0);
      transformations = parse_transformations_steps lines;
    }
  in
  Advent.log_action action_name true;
  root
