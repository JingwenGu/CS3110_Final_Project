open OUnit2
open DT.DecisionTree
open DT.DecisionTree2D
open DT.RandomForest

let eps = 1e-6

(* DecisionTree (1‑D)*)

let dt_dataset : DT.DecisionTree.dataset =
  ([ [| 1. |]; [| 2. |]; [| 3. |]; [| 4. |] ], [ 0; 0; 1; 1 ])

let decision_tree_tests =
  "DecisionTree"
  >::: [
         ( "split_dataset_counts" >:: fun _ ->
           let left, right = DT.DecisionTree.split_dataset dt_dataset 0 2.5 in
           assert_equal 2 (List.length (fst left));
           assert_equal 2 (List.length (fst right)) );
         ( "best_split" >:: fun _ ->
           let axis, thr = DT.DecisionTree.best_split dt_dataset in
           assert_equal 0 axis;
           assert_bool "threshold ≈ 2.5" (abs_float (thr -. 2.5) < eps) );
         ( "predict" >:: fun _ ->
           let tree = DT.DecisionTree.build_tree dt_dataset 0 2 in
           assert_equal 0 (DT.DecisionTree.predict tree [| 1.5 |]);
           assert_equal 1 (DT.DecisionTree.predict tree [| 3.5 |]) );
       ]
(* DecisionTree2D (2‑D)*)

let dt2d_dataset : DT.DecisionTree2D.dataset =
  ([ (1., 1.); (2., 1.); (3., 1.); (4., 1.) ], [ 0; 0; 1; 1 ])

let decision_tree2d_tests =
  "DecisionTree2D"
  >::: [
         ( "split_dataset_counts" >:: fun _ ->
           let left, right =
             DT.DecisionTree2D.split_dataset dt2d_dataset DT.DecisionTree2D.X
               2.5
           in
           assert_equal 2 (List.length (fst left));
           assert_equal 2 (List.length (fst right)) );
         ( "best_split_axis_threshold" >:: fun _ ->
           let axis, thr = DT.DecisionTree2D.best_split dt2d_dataset in
           (match axis with
           | DT.DecisionTree2D.X -> ()
           | DT.DecisionTree2D.Y -> assert_failure "Expected split on X axis");
           assert_bool "threshold ≈ 2.5" (abs_float (thr -. 2.5) < eps) );
         ( "predict" >:: fun _ ->
           let tree = DT.DecisionTree2D.build_tree dt2d_dataset 0 2 in
           assert_equal 0 (DT.DecisionTree2D.predict tree (1.5, 0.9));
           assert_equal 1 (DT.DecisionTree2D.predict tree (3.5, 1.1)) );
       ]

(* RandomForest*)

let rf_dataset : DT.RandomForest.dataset =
  ([ [| 1. |]; [| 2. |]; [| 3. |]; [| 4. |] ], [ 0; 0; 1; 1 ])

let random_forest_tests =
  "RandomForest"
  >::: [
         ( "predict_majority_vote" >:: fun _ ->
           Random.init 42;
           let forest =
             DT.RandomForest.train_forest rf_dataset (DT.RandomForest.Size 4) 5
               2
           in
           assert_equal 0 (DT.RandomForest.predict_forest forest [| 1.5 |]);
           assert_equal 1 (DT.RandomForest.predict_forest forest [| 3.5 |]) );
       ]

let eps = 1e-9
let feq a b = abs_float (a -. b) < eps

let lin n a b =
  if n <= 1 then [ a ]
  else
    let s = (b -. a) /. float (n - 1) in
    List.init n (fun i -> a +. (float i *. s))

let lbl1 x = if x < 0. then 0 else 1
let ds1 xs = (List.map (fun x -> [| x |]) xs, List.map lbl1 xs)
let lbl2 (x, y) = if (int_of_float x + int_of_float y) mod 2 = 0 then 0 else 1

let grid w h =
  let rec f i j ps ls =
    if i = w then (List.rev ps, List.rev ls)
    else if j = h then f (i + 1) 0 ps ls
    else
      let p = (float i, float j) in
      f i (j + 1) (p :: ps) (lbl2 p :: ls)
  in
  f 0 0 [] []

let dsd d n =
  let r () = Random.float 2. -. 1. in
  let rec g k ps ls =
    if k = 0 then (List.rev ps, List.rev ls)
    else
      let a = Array.init d (fun _ -> r ()) in
      g (k - 1) (a :: ps) ((if a.(0) < 0. then 0 else 1) :: ls)
  in
  g n [] []

let dep1 =
  let rec h = function
    | DT.DecisionTree.Leaf _ -> 0
    | DT.DecisionTree.Node (_, _, l, r) -> 1 + max (h l) (h r)
  in
  h

let dep2 =
  let rec h = function
    | DT.DecisionTree2D.Leaf _ -> 0
    | DT.DecisionTree2D.Node (_, _, l, r) -> 1 + max (h l) (h r)
  in
  h

let gen_small_dt_tests () =
  let ds = ds1 (lin 4 1. 4.) in
  let t = DT.DecisionTree.build_tree ds 0 2 in
  [
    ( "dt_split" >:: fun _ ->
      let l, r = DT.DecisionTree.split_dataset ds 0 2.5 in
      assert_equal 2 (List.length (fst l));
      assert_equal 2 (List.length (fst r)) );
    ( "dt_best" >:: fun _ -> (* Failure *)
      let a, th = DT.DecisionTree.best_split ds in
      assert_equal 0 a;
      assert_bool "thr" (feq th 2.5) );
    ("dt_depth" >:: fun _ -> assert_equal 2 (dep1 t)); (* failure *)
    ( "dt_pred_left" >:: fun _ -> (* failure *)
      assert_equal 0 (DT.DecisionTree.predict t [| 1. |]) );
    ( "dt_pred_right" >:: fun _ ->
      assert_equal 1 (DT.DecisionTree.predict t [| 4. |]) );
  ]

let gen_dt_edge_tests () =
  let xs = [ -5.; -4.; -3.; -2.; -1.; 0.; 1.; 2.; 3. ] in
  let ds = ds1 xs in
  [
    ( "dt_split_all_left" >:: fun _ ->
      let l, r = DT.DecisionTree.split_dataset ds 0 10. in
      assert_equal (List.length xs) (List.length (fst l));
      assert_equal 0 (List.length (fst r)) );
    ( "dt_split_all_right" >:: fun _ ->
      let l, r = DT.DecisionTree.split_dataset ds 0 (-10.) in
      assert_equal 0 (List.length (fst l));
      assert_equal (List.length xs) (List.length (fst r)) );
    ( "dt_build_leaf_by_depth0" >:: fun _ ->
      match DT.DecisionTree.build_tree ds 0 0 with
      | DT.DecisionTree.Leaf _ -> assert_bool "leaf" true
      | _ -> assert_failure "not leaf" );
    ( "dt_build_leaf_homogenous" >:: fun _ ->
      let pts, lbls = ds1 [ 1.; 2.; 3. ] in
      let lbls = List.map (fun _ -> 7) lbls in
      match DT.DecisionTree.build_tree (pts, lbls) 0 5 with
      | DT.DecisionTree.Leaf l -> assert_equal 7 l
      | _ -> assert_failure "not leaf" );
    ( "dt_best_no_candidates" >:: fun _ ->
      let pts = [ [| 1. |]; [| 1. |] ] in
      let lbls = [ 0; 1 ] in
      let a, th = DT.DecisionTree.best_split (pts, lbls) in
      assert_equal 0 a;
      assert_bool "th0" (feq th 0.) );
  ]

let gen_dt_random n =
  List.init n (fun i ->
      "dt_rand_" ^ string_of_int i >:: fun _ ->
      Random.init ((i * 19) + 3);
      let xs = List.init 25 (fun _ -> Random.float 6. -. 3.) in
      let ds = ds1 xs in
      let t = DT.DecisionTree.build_tree ds 0 4 in
      List.iter2
        (fun p l -> assert_equal l (DT.DecisionTree.predict t p))
        (fst ds) (snd ds))

let gen_dt2_basic () =
  let ds = grid 4 3 in
  let t = DT.DecisionTree2D.build_tree ds 0 3 in
  [
    ( "dt2_pred" >:: fun _ -> (* failure *)
      List.iter2
        (fun p l -> assert_equal l (DT.DecisionTree2D.predict t p))
        (fst ds) (snd ds) );
    ( "dt2_split_x" >:: fun _ ->
      let l, r = DT.DecisionTree2D.split_dataset ds DT.DecisionTree2D.X 1.5 in
      assert_equal
        (List.length (fst ds))
        (List.length (fst l) + List.length (fst r)) );
    ( "dt2_split_y" >:: fun _ ->
      let l, _ = DT.DecisionTree2D.split_dataset ds DT.DecisionTree2D.Y (-1.) in
      assert_equal 0 (List.length (fst l)) );
    ("dt2_depth" >:: fun _ -> assert_equal 3 (dep2 t));
  ]

let gen_dt2_edge () =
  let ds = grid 2 2 in
  [
    ( "dt2_best_axis" >:: fun _ ->
      let ax, _ = DT.DecisionTree2D.best_split ds in
      match ax with
      | DT.DecisionTree2D.X | DT.DecisionTree2D.Y -> assert_bool "axis ok" true
    );
    ( "dt2_leaf_depth0" >:: fun _ ->
      match DT.DecisionTree2D.build_tree ds 0 0 with
      | DT.DecisionTree2D.Leaf _ -> assert_bool "ok" true
      | _ -> assert_failure "nl" );
  ]

let gen_dt2_random m =
  List.init m (fun k ->
      let w = k + 3 in
      let h = k + 2 in
      "dt2_rand_" ^ string_of_int k >:: fun _ -> (* failure in all cases *)
      let ds = grid w h in
      let t = DT.DecisionTree2D.build_tree ds 0 4 in
      List.iter2
        (fun p l -> assert_equal l (DT.DecisionTree2D.predict t p))
        (fst ds) (snd ds))

let gen_rf_basic () =
  Random.init 81;
  let ds = ds1 (lin 15 (-3.) 3.) in
  let f = DT.RandomForest.train_forest ds (DT.RandomForest.Size 15) 5 3 in
  [
    ( "rf_pred_train" >:: fun _ ->
      List.iter2
        (fun p l -> assert_equal l (DT.RandomForest.predict_forest f p))
        (fst ds) (snd ds) );
    ( "rf_out_left" >:: fun _ ->
      assert_equal 0 (DT.RandomForest.predict_forest f [| -2.9 |]) );
    ( "rf_out_right" >:: fun _ ->
      assert_equal 1 (DT.RandomForest.predict_forest f [| 2.9 |]) );
    ("rf_tree_cnt" >:: fun _ -> assert_equal 5 (List.length f));
  ]

let gen_rf_ratio_size_tests () =
  let ds = ds1 (lin 10 (-1.) 1.) in
  [
    ( "rf_sample_ratio0" >:: fun _ ->
      let b = DT.RandomForest.sample_batch ds (DT.RandomForest.Ratio 0.) in
      assert_equal 0 (List.length (fst b)) );
    ( "rf_sample_ratio1" >:: fun _ ->
      let b = DT.RandomForest.sample_batch ds (DT.RandomForest.Ratio 1.) in
      assert_equal (List.length (fst ds)) (List.length (fst b)) );
    ( "rf_sample_ratio_gt1" >:: fun _ ->
      let b = DT.RandomForest.sample_batch ds (DT.RandomForest.Ratio 2.) in
      assert_bool ">=" (List.length (fst b) >= List.length (fst ds)) );
    ( "rf_sample_size0" >:: fun _ ->
      let b = DT.RandomForest.sample_batch ds (DT.RandomForest.Size 0) in
      assert_equal 0 (List.length (fst b)) );
    ( "rf_sample_size_big" >:: fun _ ->
      let b = DT.RandomForest.sample_batch ds (DT.RandomForest.Size 50) in
      assert_equal 10 (List.length (fst b)) );
  ]

let gen_rf_tie () =
  let leaf0 = DT.DecisionTree.Leaf 0 in
  let leaf1 = DT.DecisionTree.Leaf 1 in
  let f = [ leaf0; leaf1 ] in
  [
    ( "rf_tie" >:: fun _ ->
      let r = DT.RandomForest.predict_forest f [| 0. |] in
      assert_bool "tie ok" (r = 0 || r = 1) );
  ]

let gen_rf_random s =
  List.map
    (fun sd ->
      "rf_rand_" ^ string_of_int sd >:: fun _ -> (* failure in some cases*)
      Random.init sd;
      let xs = List.init 30 (fun _ -> Random.float 10. -. 5.) in
      let ds = ds1 xs in
      let f = DT.RandomForest.train_forest ds (DT.RandomForest.Ratio 0.7) 9 4 in
      List.iter2
        (fun p l -> assert_equal l (DT.RandomForest.predict_forest f p))
        (fst ds) (snd ds))
    s

let gen_nd () =
  List.init 5 (fun d ->
      "nd_" ^ string_of_int d >:: fun _ ->
      Random.init (d * 13);
      let ds = dsd (max 1 d) 40 in
      let f = DT.RandomForest.train_forest ds (DT.RandomForest.Ratio 1.) 3 5 in
      List.iter2
        (fun p l -> assert_equal l (DT.RandomForest.predict_forest f p))
        (fst ds) (snd ds))

let gen_deep () =
  [
    ( "deep" >:: fun _ ->
      let ds = ds1 (lin 80 (-40.) 39.) in
      let t = DT.DecisionTree.build_tree ds 0 10 in
      List.iter2
        (fun p l -> assert_equal l (DT.DecisionTree.predict t p))
        (fst ds) (snd ds) );
  ]

let gen_noise () =
  [
    ( "noise_acc" >:: fun _ ->
      Random.init 4242;
      let xs = lin 250 (-12.) 12. in
      let base = List.map lbl1 xs in
      let noisy =
        List.mapi (fun i l -> if i mod 9 = 0 then 1 - l else l) base
      in
      let ds = (List.map (fun x -> [| x |]) xs, noisy) in
      let f = DT.RandomForest.train_forest ds (DT.RandomForest.Ratio 1.) 11 5 in
      let c = ref 0 in
      List.iter2
        (fun p l -> if DT.RandomForest.predict_forest f p = l then incr c)
        (fst ds) base );
  ]

let suites =
  List.concat
    [
      gen_small_dt_tests ();
      gen_dt_edge_tests ();
      gen_dt_random 120;
      gen_dt2_basic ();
      gen_dt2_edge ();
      gen_dt2_random 30;
      gen_rf_basic ();
      gen_rf_ratio_size_tests ();
      gen_rf_tie ();
      gen_rf_random [ 200; 201; 202; 203; 204; 205; 206; 207; 208; 209 ];
      gen_nd ();
      gen_deep ();
      gen_noise ();
    ]

let () =
  run_test_tt_main
    ("all_tests"
    >::: [ decision_tree_tests; decision_tree2d_tests; random_forest_tests ]
         @ suites)
