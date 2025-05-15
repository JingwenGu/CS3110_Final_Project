(** [print_list list] prints [list] *)
val print_list : int list -> unit

(** [count_occurrences list] is the counter dictionary for [list] where each element is a tuple of the list element and its count *)
val count_occurrences : 'a list -> ('a * int) list

(** [majority_label labels] is the label that appears the most often in [labels] *)
val majority_label : int list -> int

(** [entropy list] is the entropy value of [list] *)
val entropy : 'a list -> float

(** [pl2lp (list1, list2)] is the list where each element is the tuple consisting of corresponding elements in [list1] and [list2] *)
val pl2lp : 'a list * 'b list -> ('a * 'b) list

(** [lp2pl list] is the tuple [(list1,list2)] where [list1],[list2] consist of the first and second element of each element of [list] respectively*)
val lp2pl : ('a * 'b) list -> 'a list * 'b list