open OUnit2
open DT.DecisionTree
open DT.DecisionTree2D
open DT.RandomForest

let eps = 1e-6

(* DecisionTree (1‑D)*)

let dt_dataset : DT.DecisionTree.dataset =
  ( [ [|1.|]; [|2.|]; [|3.|]; [|4.|] ],  (* points *)
    [ 0; 0; 1; 1 ] )                     (* labels *)

let decision_tree_tests =
  "DecisionTree" >::: [
    "split_dataset_counts" >:: (fun _ ->
      let left, right = DT.DecisionTree.split_dataset dt_dataset 0 2.5 in
      assert_equal 2 (List.length (fst left));
      assert_equal 2 (List.length (fst right))
    );

    "best_split" >:: (fun _ ->
      let axis, thr = DT.DecisionTree.best_split dt_dataset in
      assert_equal 0 axis;
      assert_bool "threshold ≈ 2.5" (abs_float (thr -. 2.5) < eps)
    );

    "predict" >:: (fun _ ->
      let tree = DT.DecisionTree.build_tree dt_dataset 0 2 in
      assert_equal 0 (DT.DecisionTree.predict tree [|1.5|]);
      assert_equal 1 (DT.DecisionTree.predict tree [|3.5|])
    );
  ]
(* DecisionTree2D (2‑D)*)

let dt2d_dataset : DT.DecisionTree2D.dataset =
  ( [ (1., 1.); (2., 1.); (3., 1.); (4., 1.) ],
    [ 0; 0; 1; 1 ] )

let decision_tree2d_tests =
  "DecisionTree2D" >::: [
    "split_dataset_counts" >:: (fun _ ->
      let left, right = DT.DecisionTree2D.split_dataset dt2d_dataset DT.DecisionTree2D.X 2.5 in
      assert_equal 2 (List.length (fst left));
      assert_equal 2 (List.length (fst right))
    );

    "best_split_axis_threshold" >:: (fun _ ->
      let axis, thr = DT.DecisionTree2D.best_split dt2d_dataset in
      (match axis with
       | DT.DecisionTree2D.X -> ()
       | DT.DecisionTree2D.Y -> assert_failure "Expected split on X axis");
      assert_bool "threshold ≈ 2.5" (abs_float (thr -. 2.5) < eps)
    );

    "predict" >:: (fun _ ->
      let tree = DT.DecisionTree2D.build_tree dt2d_dataset 0 2 in
      assert_equal 0 (DT.DecisionTree2D.predict tree (1.5, 0.9));
      assert_equal 1 (DT.DecisionTree2D.predict tree (3.5, 1.1))
    );
  ]

(* RandomForest*)

let rf_dataset : DT.RandomForest.dataset =
  ( [ [|1.|]; [|2.|]; [|3.|]; [|4.|] ],
    [ 0; 0; 1; 1 ] )

let random_forest_tests =
  "RandomForest" >::: [
    "predict_majority_vote" >:: (fun _ ->
      Random.init 42;  (* deterministic sampling *)
      let forest = DT.RandomForest.train_forest rf_dataset (DT.RandomForest.Size 4) 5 2 in
      assert_equal 0 (DT.RandomForest.predict_forest forest [|1.5|]);
      assert_equal 1 (DT.RandomForest.predict_forest forest [|3.5|])
    );
  ]



let () =
  run_test_tt_main (
    "all_tests" >::: [
      decision_tree_tests;
      decision_tree2d_tests;
      random_forest_tests;
    ])
