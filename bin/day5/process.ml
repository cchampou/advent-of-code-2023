let rec apply_transformations seed (t : Types.transformation list) =
  match t with
  | [] -> seed
  | h :: tail ->
      let offset = h.destination - h.source in
      Printf.printf "seed %d offset %d\n" seed offset;
      if seed >= h.source && seed < h.source + h.range then (
        Logger.log_transformation_unit h;
        print_char '\n';
        let res = seed + offset in
        Printf.printf "seed %d matches, returning %d\n" seed res;
        res)
      else apply_transformations seed tail

let rec process_seed seed transformation_steps =
  match transformation_steps with
  | [] -> seed
  | h :: t ->
      let new_seed = apply_transformations seed h in
      process_seed new_seed t

let rec process_seeds seeds transformation_steps =
  match seeds with
  | [] -> []
  | h :: t ->
      process_seed h transformation_steps
      :: process_seeds t transformation_steps

let process_root (root : Types.root) =
  let res = process_seeds root.seeds root.transformations in
  let seeds_processed = res |> List.sort (fun a b -> if a < b then a else b) in
  Printf.printf "Result: %d\n" (List.nth seeds_processed 0)
