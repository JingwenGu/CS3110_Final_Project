(** The type for points to be classified *)
type point = float * float

(** The type of labels *)
type label = int

(** The type of datasets *)
type dataset = point list * label list

(** The type representing which axis to split along *)
type split_axis = X | Y

(** The type representing the decision tree *)
type decision_tree =
| Leaf of label
| Node of split_axis * float * decision_tree * decision_tree

(** [split_dataset dataset axis threshold] is the tuple consisting of two parts of [dataset] split on [axis] and [threshold] *)
val split_dataset : dataset -> split_axis -> float -> dataset * dataset

(** [best_split (points,labels)] is the tuple where the first element is the optimal split axis and the second element is the associated threshold *)
val best_split : point list * label list -> split_axis * float

(** [build_tree (points,labels) depth max_depth] is the decision tree trained on data [(points,labels)] with hyperparameters [depth] and [max_depth] *)
val build_tree : point list * label list -> int -> int -> decision_tree

(** [predict tree point] is the predicted label of [point] by [tree] *)
val predict : decision_tree -> float * float -> label
