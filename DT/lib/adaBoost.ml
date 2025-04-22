(**adaBoost.ml*)
type adaboost_sample = float array
type adaboost_label = int (** +1 or -1 *)
type adaboost_dataset = adaboost_sample list * adaboost_label list

(** Decision stump: one feature, threshold, and polarity *)
type adaboost_stump = {
  stump_axis : int;
  stump_thresh : float;
  stump_polarity: int; (** +1 or -1 *)
}

(** Ensemble is a list of (stump, weight) pairs *)
type adaboost_ensemble = (adaboost_stump * float) list

(** Predict with a single stump *)
let adaboost_predict_stump {stump_axis; stump_thresh; stump_polarity} (x: adaboost_sample) : int =
  let h = if x.(stump_axis) *. float_of_int stump_polarity <= stump_thresh *. float_of_int stump_polarity
          then 1 else -1
  in h

let safe_thresholds values =
  match values with
  | [] | [_] -> values
  | _ ->
      let rec aux prev rest acc =
        match rest with
        | [] -> acc
        | hd :: tl -> 
            let mid = (prev +. hd) /. 2.0 in
            aux hd tl (mid :: acc)
      in
      let sorted = List.sort compare values in
      aux (List.hd sorted) (List.tl sorted) []

(** Find the best stump for given weights *)
let adaboost_train_stump ((xs, ys): adaboost_dataset) (weights: float array) : adaboost_stump =
  let dims = Array.length (List.hd xs) in
  
  (** Collect sorted unique feature values on axis d *)
  let values d =
    xs
    |> List.map (fun x -> x.(d))
    |> List.sort_uniq compare
  in
  
  (** Evaluate a candidate stump *)
  let eval_candidate axis thresh polarity =
    let err = ref 0. in
    List.iteri (fun i x ->
      let y = List.nth ys i in
      let pred = if (x.(axis) *. float_of_int polarity) <= (thresh *. float_of_int polarity)
                then 1 else -1
      in
      if pred <> y then err := !err +. weights.(i)
    ) xs;
    !err
  in
  
  (** Search for best (axis, thresh, polarity) *)
  let best = ref (0, 0., 1, infinity) in
  for axis = 0 to dims - 1 do
    let vs = values axis in
    let thresholds = safe_thresholds vs in
    
    List.iter (fun thresh ->
      List.iter (fun polarity ->
        let err = eval_candidate axis thresh polarity in
        let (_, _, _, best_err) = !best in
        if err < best_err then best := (axis, thresh, polarity, err)
      ) [1; -1]
    ) thresholds
  done;
  let (axis, thresh, polarity, _) = !best in
  { stump_axis = axis; stump_thresh = thresh; stump_polarity = polarity }

(** Train AdaBoost for [rounds] weak learners *)
let adaboost_train_ada ((xs, ys) as data: adaboost_dataset) rounds : adaboost_ensemble =
  let n = List.length xs in
  let weights = Array.make n (1.0 /. float_of_int n) in
  let ensemble = ref [] in
  for _ = 1 to rounds do
    (** Fit stump to weighted data *)
    let stump = adaboost_train_stump data weights in
    
    (**  weighted error *)
    let err = ref 0. in
    List.iteri (fun i x ->
      let pred = adaboost_predict_stump stump x in
      if pred <> List.nth ys i then
        err := !err +. weights.(i)
    ) xs;
    
    (** avoid division by zero / bad stumps *)
    if !err >= 0.5 then () else begin
      (**  alpha *)
      let alpha = 0.5 *. log ((1.0 -. !err) /. !err) in
      
      (** update weights *)
      let z = ref 0. in
      List.iteri (fun i x ->
        let y = float_of_int (List.nth ys i) in
        let h = float_of_int (adaboost_predict_stump stump x) in
        let w = weights.(i) *. exp (-. alpha *. y *. h) in
        weights.(i) <- w;
        z := !z +. w
      ) xs;
      
      (** normalize *)
      for i = 0 to n - 1 do
        weights.(i) <- weights.(i) /. !z
      done;
      
      ensemble := (stump, alpha) :: !ensemble
    end
  done;
  List.rev !ensemble

(** Predict with the final ensemble *)
let adaboost_predict_ada (ens: adaboost_ensemble) (x: adaboost_sample) : int =
  let sum = List.fold_left (fun acc (st, alpha) ->
    acc +. alpha *. float_of_int (adaboost_predict_stump st x)
  ) 0. ens in
  if sum >= 0. then 1 else -1

(** Convert 0/1 labels to -1/+1 for AdaBoost *)
let adaboost_convert_labels labels =
  List.map (fun l -> if l = 0 then -1 else 1) labels

(** Convert -1/+1 predictions back to 0/1 *)
let adaboost_convert_prediction pred =
  if pred = -1 then 0 else 1
