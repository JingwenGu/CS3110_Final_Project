type point = float array
type label = int
type dataset = point list * label list
type split_axis = int

type decision_tree =
  | Leaf of label
  | Node of split_axis * float * decision_tree * decision_tree

type decision_forest = decision_tree list

type sample_factor = Ratio of float | Size of int

let sample_batch dataset sample_f =
  let sample_size = match sample_f with
    | Ratio r -> ((dataset |> fst |> List.length |> float_of_int) *. r) |> int_of_float
    | Size n -> n
  in

  let datalist = Utils.pl2lp dataset in

  let shuffle lst =
    let arr = Array.of_list lst in
    let len = Array.length arr in
    for i = len - 1 downto 1 do
      let j = Random.int (i + 1) in
      let tmp = arr.(i) in
      arr.(i) <- arr.(j);
      arr.(j) <- tmp;
    done;
    Array.to_list arr
  in

  let random_sample_without_replacement lst n =
    let shuffled = shuffle lst in
    let rec take acc i = function
      | [] -> List.rev acc
      | _ when i = 0 -> List.rev acc
      | x :: xs -> take (x :: acc) (i - 1) xs
    in
    take [] n shuffled 
  in
  
  (random_sample_without_replacement datalist sample_size) |> Utils.lp2pl

let train_forest dataset sample_f n_trees max_depth=
  let batches = List.init n_trees (fun _ -> sample_batch dataset sample_f) in
  List.map (fun batch -> DecisionTree.build_tree batch 0 max_depth) batches

let predict_forest forest point =
  let labels = List.map (fun tree -> DecisionTree.predict tree point) forest in
  Utils.majority_label labels