(** The type for points to be classified *)
type point = float array

(** The type of labels *)
type label = int

(** The type of datasets *)
type dataset = point list * label list

(** The type representing which axis to split along *)
type split_axis = int

(** The type representing the decision tree *)
type decision_tree =
  | Leaf of label
  | Node of split_axis * float * decision_tree * decision_tree

(** The type representing the decision forest *)
type decision_forest = decision_tree list

(** The type representing the sampling factor *)
type sample_factor = Ratio of float | Size of int

(** [sample_batch (list1,list2) factor] is the batch sampled from [(list1,list2)] with sampling factor [factor] *)
val sample_batch : 'a list * 'b list -> sample_factor -> 'a list * 'b list

(** [train_forest dataset factor depth max_depth] is the random forest trained on [dataset] 
    with sampling factor [factor] and each tree with [depth] and [max_depth] *)
val train_forest : dataset -> sample_factor -> int -> int -> DecisionTree.decision_tree list

(** [predict forest point] is the predicted label of [point] by [forest] *)
val predict_forest : DecisionTree.decision_tree list -> point -> label