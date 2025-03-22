(* open DT *)

let () =
  let dataset = [
    (1.0, 1.0); (2.0, 1.5); (1.5, 2.0);  (* bottom-left quadrant → class 0 *)
    (1.0, 9.0); (2.0, 8.5); (1.5, 7.5);  (* top-left quadrant    → class 1 *)
    (8.0, 1.0); (9.0, 1.5); (7.5, 2.0);  (* bottom-right         → class 1 *)
    (8.0, 9.0); (9.0, 8.0); (7.5, 7.5);  (* top-right            → class 0 *)
    (5.0, 5.0); (5.5, 5.5); (4.5, 4.5)   (* center cluster       → class 0 *)
  ] in
  let labels = [
    0; 0; 0;
    1; 1; 1;
    1; 1; 1;
    0; 0; 0;
    0; 0; 0
  ] in
  let tree = DT.DecisionTree.build_tree (dataset, labels) 0 3 in

  let test_points = [(2.5, 2.5); (7.0,3.0); (7.5, 9.0); (0.5,9.5); (5.0, 5.0)] in
  List.iter (fun pt ->
    let label = DT.DecisionTree.predict tree pt in
    Printf.printf "Point (%f, %f) => Class %d\n" (fst pt) (snd pt) label
  ) test_points;
  let mesh = { DT.Display2D.x_min = 0.0; y_min = 0.0; x_max = 10.0; y_max = 10.0; x_unit = 0.5; y_unit = 0.5 } in
  let _ = DT.Display2D.plot_DT mesh (fun pt ->float_of_int(DT.DecisionTree.predict tree pt)) DT.Display2D.dict_demo in ()
