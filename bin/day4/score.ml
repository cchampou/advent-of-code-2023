let rec calc_score score ((win, play) : int list * int list) =
  match play with
  | [] -> score
  | head :: tail -> (
      try
        let _ = List.find (fun x -> x = head) win in
        calc_score (score + 1) (win, tail)
        (* if score = 0 then
             calc_score 1 (win,tail)
           else
             calc_score (score * 2) (win,tail) *)
      with _ -> calc_score score (win, tail))

let add_score_occurence (win, play) =
  ((win, play), (calc_score 0 (win, play), 1))

let rec alter_occurence (l : ((int list * int list) * (int * int)) list) score
    occ : ((int list * int list) * (int * int)) list =
  if score = 0 || List.length l = 0 then l
  else
    match l with
    | [] -> l
    | (data, (curr_score, curr_occ)) :: tail ->
        Printf.printf "Adding %d to the card %d %d\n" occ curr_score curr_occ;
        (data, (curr_score, curr_occ + occ))
        :: alter_occurence tail (score - 1) occ

let rec distribute_occurences l : ((int list * int list) * (int * int)) list =
  match l with
  | [] -> l
  | head :: tail ->
      let _, (score, occ) = head in
      Printf.printf "Processing card with score %d and occurence %d\n" score occ;
      let modified_tail = alter_occurence tail score occ in
      head :: distribute_occurences modified_tail

let rec count_occurences score l : int =
  match l with
  | [] -> score
  | head :: tail ->
      let _, (_, occ) = head in
      count_occurences (score + occ) tail
