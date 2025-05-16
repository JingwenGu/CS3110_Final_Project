type point = float array
type label = int
type dataset = point list * label list

type split_axis = int
type decision_tree =
  | Leaf of label
  | Node of split_axis * float * decision_tree * decision_tree
(** AF: [Node(axis,threshold,left,right)] is the tree node with split axis [axis], threshold value [thershold], and subtrees [left,right].
        [Leaf(label)] is the leaf node where the prediction is always [label].
    RI: All points in [left] must have their values for [axis] at most [threshold], 
        while all points in [left] must have their values for [axis] at least [threshold] *)

(* Split dataset on axis and threshold *)
let split_dataset ((points, labels) : dataset) (axis : split_axis) (threshold : float) : dataset * dataset =
  List.fold_left2 (fun ((l_pts, l_lbls), (r_pts, r_lbls)) pt lbl ->
    let value = pt.(axis) in
    if value <= threshold then
      (( pt :: l_pts, lbl :: l_lbls ), (r_pts, r_lbls))
    else
      ((l_pts, l_lbls), ( pt :: r_pts, lbl :: r_lbls ))
  ) (([], []), ([], [])) points labels

(* Try all candidate splits and find the best *)
let best_split (points, labels) =
  let dim = match points with
    | [] -> 0
    | h :: _ -> Array.length h
  in

  (* Helper: generate adjacent pairs from a sorted list *)
  let rec pairs lst =
    match lst with
    | a :: (b :: _ as rest) -> (a, b) :: pairs rest
    | _ -> []
  in

  let rec axes n = 
    if n = 0 then [0]
    else n :: axes (n-1) 
  in

  (* Generate candidate (axis, threshold) pairs *)
  let candidates =
    List.flatten (
      List.map (fun axis ->
        let values = List.map (fun pt -> pt.(axis)) points in
        let sorted = List.sort_uniq compare values in
        let thresholds = List.map (fun (a, b) -> (a +. b) /. 2.0) (pairs sorted) in
        List.map (fun t -> (axis, t)) thresholds
      ) (axes (dim-1))
    )
  in

  let total_entropy = Utils.entropy labels in
  (* Handle edge case where no valid candidates exist *)
  match candidates with
  | [] -> (0, 0.0)  (* Fallback split; could also return an option for safety *)
  | _ ->
      let best_axis, best_thresh, _ =
        List.fold_left (fun (best_axis, best_thresh, best_gain) (axis, t) ->
          let (left, right) = split_dataset (points, labels) axis t in
          let l_lbls = snd left and r_lbls = snd right in
          let size = float_of_int (List.length labels) in
          let l_size = float_of_int (List.length l_lbls) in
          let r_size = float_of_int (List.length r_lbls) in

          (* Skip empty splits *)
          if l_size = 0.0 || r_size = 0.0 then (best_axis, best_thresh, best_gain)
          else
            let gain =
              total_entropy
              -. (l_size /. size) *. Utils.entropy l_lbls
              -. (r_size /. size) *. Utils.entropy r_lbls
            in
            if gain > best_gain then (axis, t, gain)
            else (best_axis, best_thresh, best_gain)
        ) (0, 0.0, -.1.0) candidates
      in
      (best_axis, best_thresh)

(* Build decision tree recursively *)
let rec build_tree (points, labels) (depth:int) (max_depth:int) =
  match Utils.count_occurrences labels with
  | [(l, _)] -> Leaf l
  | _ when depth >= max_depth || List.length labels < 2 -> Leaf (Utils.majority_label labels)
  | _ ->
      let axis, threshold = best_split (points, labels) in
      let (left, right) = split_dataset (points, labels) axis threshold in
      let left_tree = build_tree left (depth + 1) max_depth in
      let right_tree = build_tree right (depth + 1) max_depth in
      Node (axis, threshold, left_tree, right_tree)

(* Prediction *)
let rec predict (tree : decision_tree) (pt : point) : label =
  match tree with
  | Leaf l -> l
  | Node (axis, threshold, left, right) ->
      let value = pt.(axis) in
      if value <= threshold then predict left pt else predict right pt