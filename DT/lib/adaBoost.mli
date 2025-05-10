(** adaboost.mli *)

(** A sample is a feature vector of floats. *)
type adaboost_sample = float array

(** Labels are +1 or -1 for AdaBoost. *)
type adaboost_label = int

(** A dataset is a list of samples paired with a list of labels. *)
type adaboost_dataset = adaboost_sample list * adaboost_label list

(** A decision stump is defined by:
    - [stump_axis]: which feature index to test
    - [stump_thresh]: the threshold value
    - [stump_polarity]: +1 or -1 indicating direction of the inequality
*)
type adaboost_stump = {
  stump_axis : int;
  stump_thresh : float;
  stump_polarity : int;
}

(** An ensemble is a list of (stump × weight) pairs. *)
type adaboost_ensemble = (adaboost_stump * float) list

(** [adaboost_predict_stump stump x] predicts +1 or –1 on sample [x] using [stump]. *)
val adaboost_predict_stump :  adaboost_stump -> adaboost_sample -> int

(** [adaboost_train_stump dataset weights] finds the best stump
    minimizing weighted classification error on [dataset] under [weights]. *)
val adaboost_train_stump :  adaboost_dataset -> float array -> adaboost_stump

(** [adaboost_train_ada dataset rounds] trains AdaBoost for [rounds]
    weak learners on [dataset], returning the final ensemble. *)
val adaboost_train_ada :  adaboost_dataset -> int -> adaboost_ensemble

(** [adaboost_predict_ada ens x] predicts 0 or 1 on sample [x] by
    combining the ensemble [ens]. *)
val adaboost_predict_ada :  adaboost_ensemble -> adaboost_sample -> int

(** [adaboost_convert_labels labels] converts a list of 0/1 labels
    into –1/+1 for AdaBoost training. *)
val adaboost_convert_labels:  int list -> adaboost_label list

(** [adaboost_convert_prediction pred] converts an AdaBoost prediction
    (–1/+1) back into 0/1. *)
val adaboost_convert_prediction :  int -> int

(** [safe_thresholds values] returns the midpoints between sorted unique floats in [values]. *)
val safe_thresholds : float list -> float list
