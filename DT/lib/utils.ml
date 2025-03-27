let count_occurrences labels =
  List.fold_left (fun acc l ->
    let count = try List.assoc l acc with Not_found -> 0 in
    (l, count + 1) :: List.remove_assoc l acc
  ) [] labels

let majority_label labels =
  fst (List.hd (List.sort (fun (_, a) (_, b) -> compare b a) (count_occurrences labels)))

let entropy labels =
  let total = float_of_int (List.length labels) in
  let counts = count_occurrences labels in
  List.fold_left (fun acc (_, count) ->
    let p = float_of_int count /. total in
    acc -. p *. log p /. log 2.0
  ) 0.0 counts

let pl2lp dataset = List.map2 (fun x y -> (x,y)) (fst dataset) (snd dataset)

let lp2pl dataset = List.fold_right (fun (x,y) (l1,l2) -> (x::l1,y::l2)) dataset ([],[])