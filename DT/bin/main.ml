open DT
(* 
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

let ds = grid 4 3
let t = DT.DecisionTree2D.build_tree ds 0 10

let predicted = List.iter2 (fun p l ->(print_endline ((string_of_float (fst p)) ^ "," ^ (string_of_float (snd p)) ^ "->" ^ (string_of_int l) ^ ":" ^ (string_of_int (DT.DecisionTree2D.predict t p))))) (fst ds) (snd ds) *)

type row = {
  id     : string;
  values : string array;
}

type dataset = {
  features : string array;
  rows      : row list;
}

(* Simple comma-split; for production use ocaml-csv *)
let split_line (line : string) : string array =
  String.split_on_char ',' line |> Array.of_list

(* Read CSV: first col is ID, first row is header *)
let read_csv (filename : string) : dataset =
  let ic = open_in filename in
  let header = input_line ic |> split_line in
  if Array.length header < 2 then
    failwith "CSV must have at least an ID column and one feature column";
  let features = Array.sub header 1 (Array.length header - 1) in
  let rows = ref [] in
  (try
      while true do
        let line = input_line ic in
        if String.trim line <> "" then
          let fields = split_line line in
          if Array.length fields <> Array.length header then
            failwith
              (Printf.sprintf
                "Bad row: expected %d fields but got %d in %s"
                (Array.length header)
                (Array.length fields) line);
          let id = fields.(0) in
          let values = Array.sub fields 1 (Array.length fields - 1) in
          rows := { id; values } :: !rows
      done
    with End_of_file -> ());
  close_in ic;
  { features; rows = List.rev !rows }

(* Convert dataset, handling missing values:
    - Numeric columns: fill missing with mean
    - Categorical: fill missing with majority, then map to random int codes *)
let convert_dataset (ds : dataset)
  : bool array * (string, int) Hashtbl.t array * (string * float array) list =
  Random.self_init ();  
  let m = Array.length ds.features in

  (* Detect numeric vs categorical (ignore missing="") *)
  let is_numeric = Array.make m true in
  List.iter
    (fun { values; _ } ->
      Array.iteri
        (fun j v ->
          if v <> "" && is_numeric.(j) then
            match (try Some (float_of_string v) with _ -> None) with
            | Some _ -> ()
            | None -> is_numeric.(j) <- false)
        values)
    ds.rows;

  (* Compute column sums/counts and category frequencies *)
  let sum        = Array.make m 0.0 in
  let cnt        = Array.make m 0   in
  let cat_counts = Array.init m (fun _ -> Hashtbl.create 16) in
  List.iter
    (fun { values; _ } ->
      Array.iteri
        (fun j v ->
          if v = "" then ()
          else if is_numeric.(j) then (
            let f = float_of_string v in
            sum.(j) <- sum.(j) +. f;
            cnt.(j) <- cnt.(j) + 1)
          else (
            let tbl = cat_counts.(j) in
            Hashtbl.replace tbl v (1 + (try Hashtbl.find tbl v with Not_found -> 0))))
        values)
    ds.rows;
  let default_num = Array.make m 0.0 in
  let default_cat = Array.make m "" in
  for j = 0 to m - 1 do
    if is_numeric.(j) then
      default_num.(j) <-
        if cnt.(j) > 0 then sum.(j) /. float_of_int cnt.(j) else 0.0
    else (
      let tbl = cat_counts.(j) in
      let maj, _ =
        Hashtbl.fold
          (fun k v (acc_k, acc_v) -> if v > acc_v then (k, v) else (acc_k, acc_v))
          tbl
          ("", -1)
      in
      default_cat.(j) <- maj)
  done;

  (* Assign codes and convert rows *)
  let maps = Array.init m (fun _ -> Hashtbl.create 16) in
  let data =
    List.map
      (fun { id; values } ->
        let arr =
          Array.mapi
            (fun j v ->
              if is_numeric.(j) then
                if v = "" then default_num.(j)
                else float_of_string v
              else
                let vstr = if v = "" then default_cat.(j) else v in
                let tbl = maps.(j) in
                match Hashtbl.find_opt tbl vstr with
                | Some c -> float_of_int c
                | None ->
                  let rec fresh () =
                    let c0 = Random.int 1_000_000 in
                    if Hashtbl.fold (fun _ c seen -> seen || c = c0) tbl false
                    then fresh ()
                    else c0
                  in
                  let c1 = fresh () in
                  Hashtbl.add tbl vstr c1;
                  float_of_int c1)
            values
        in
        (id, arr))
      ds.rows
  in
  (is_numeric, maps, data)

(* For decision tree input formats *)
type tree_input =
  | Train of float array list * int list
  | Test of float array list

let prepare_test_for_tree data = match data with
  | Test features -> List.map (fun x -> x) features
  | Train _ -> failwith "Should be test dataset instead of train dataset"

(** Convert raw data to either Train (features * labels) or Test features list.
    [label_column] is the index of the label within each feature array,
    or [-1] for a test set (no labels).
*)
let prepare_for_tree label_column data =
  let xs, ys =
    List.fold_right
      (fun (_id, arr) (acc_x, acc_y) ->
        let label =
          try int_of_float arr.(label_column)
          with _ ->
            failwith
              (Printf.sprintf "Label not integer at column %d" label_column)
        in
        let feats =
          Array.init (Array.length arr - 1) (fun i ->
            if i < label_column then arr.(i) else arr.(i + 1))
        in
        (feats :: acc_x, label :: acc_y))
      data
      ([], [])
  in
  Train (xs, ys)


let kaggle_test _ = 
  let train_data = read_csv "data/train.csv" in
  let (b,h,l) = convert_dataset train_data in
  let tree_input = prepare_for_tree 0 l in 
  let forest = match tree_input with
    | Train (features,labels) -> DT.RandomForest.train_forest (features,labels) (DT.RandomForest.Ratio 0.6) 100 50
    | Test test -> failwith "Should be a train dataset instead of a test dataset"
  in
  let test_data = read_csv "data/test.csv" in
  let (b',h',l') = convert_dataset test_data in

  let preds = List.map (fun point -> DT.RandomForest.predict_forest forest (snd point)) l' in

  let _ = List.map (fun x -> print_endline (string_of_int x)) preds in

  let write_list_to_csv ~filename xs =
    let oc = open_out filename in
    let _ = Printf.fprintf oc "PassengerId,Survived\n" in
    List.iteri
      (fun idx x ->
        let i = idx + 892 in
        Printf.fprintf oc "%d,%d\n" i x
      ) xs;
    close_out oc
  in
  write_list_to_csv ~filename:"data/preds.csv" preds

open Graphics
open Printf

(** Window and World Settings *)
let window_width = 800
let window_height = 800

(** For decision tree (2D version and array-based version) *)
let world_x_min = 0.0
let world_y_min = 0.0
let world_x_max = 10.0
let world_y_max = 10.0
let world_width = world_x_max -. world_x_min
let world_height = world_y_max -. world_y_min

let rec wait_for_space_or_q () =
  let status = wait_next_event [Key_pressed] in
  if status.key = ' ' || status.key = 'q' then status.key
  else wait_for_space_or_q ()

let rec wait_for_key_of keys =
  let ev = wait_next_event [Key_pressed] in
  if List.mem ev.key keys then ev.key else wait_for_key_of keys
let wait_for_mode () = wait_for_key_of ['i'; 'd'; 't']
let wait_for_algo () = wait_for_key_of ['d'; 'a'; 'r'; 'q']

let draw_center_instructions text =
  let rect_width = window_width / 2 in
  let rect_height = 100 in
  let rect_x = (window_width - rect_width) / 2 in
  let rect_y = (window_height - rect_height) / 2 in
  set_color white;
  fill_rect rect_x rect_y rect_width rect_height;
  set_color black;
  moveto (rect_x + 10) (rect_y + (rect_height / 2));
  draw_string text

let world_to_screen (x, y) =
  let sx = int_of_float (((x -. world_x_min) /. world_width) *. float_of_int window_width) in
  let sy = int_of_float (((y -. world_y_min) /. world_height) *. float_of_int window_height) in
  (sx, sy)

(** For random forest on spiral data *)
let spiral_x_min = -15.0
let spiral_y_min = -15.0
let spiral_x_max = 15.0
let spiral_y_max = 15.0
let spiral_width = spiral_x_max -. spiral_x_min
let spiral_height = spiral_y_max -. spiral_y_min

let world_to_screen_spiral (x, y) =
  let sx = int_of_float (((x -. spiral_x_min) /. spiral_width) *. float_of_int window_width) in
  let sy = int_of_float (((y -. spiral_y_min) /. spiral_height) *. float_of_int window_height) in
  (sx, sy)

(**Color Mapping*)
let color_of_label label =
  match label with
  | 0 -> red
  | 1 -> blue
  | _ -> green

(** Drawing Functions*)

(** Draw a decision boundary on the current Graphics window.
   f is a function mapping world coordinates to a label. *)
let draw_decision_boundary world_min_x world_min_y world_width world_height f =
  let step = 5 in
  for i = 0 to (window_width - 1) / step do
    for j = 0 to (window_height - 1) / step do
      let x = i * step in
      let y = j * step in
      let wx = (float_of_int x) /. float_of_int window_width *. world_width +. world_min_x in
      let wy = (float_of_int y) /. float_of_int window_height *. world_height +. world_min_y in
      let label = f (wx, wy) in
      set_color (color_of_label label);
      fill_rect x y step step
    done
  done

(** Draw a marker at a given world coordinate, along with the predicted label *)
let draw_marker (x, y) label to_screen =
let (sx, sy) = to_screen (x, y) in
set_color black;
fill_circle sx sy 5;
set_color white;  
moveto (sx + 10) (sy + 10);
draw_string (sprintf "Label: %d" label)

let interactive_mode () =
  (** storage for points + labels, and current class *)
  let pts = ref [] in
  let labs = ref [] in
  let current_class = ref 0 in

  (** choose algorithm *)
  clear_graph ();
  draw_center_instructions
    "Interactive: d=DecisionTree, a=AdaBoost, r=RandomForest, q=quit";
  let algo = wait_for_algo () in
  if algo = 'q' then () else

  (** train_fn returns a function world->label *)
  let train_fn () =
    match algo with
    | 'd' ->
      let data = List.map (fun (x,y) -> [|x;y|]) !pts in
      let tree = DT.DecisionTree.build_tree (data, !labs) 0 5 in
      fun (x,y) -> DT.DecisionTree.predict tree [|x;y|]
    | 'a' ->
      let data = List.map (fun (x,y) -> [|x;y|]) !pts in
      let ys_ = DT.AdaBoost.adaboost_convert_labels !labs in
      let ens = DT.AdaBoost.adaboost_train_ada (data, ys_) 20 in
      fun (x,y) ->
        let p = DT.AdaBoost.adaboost_predict_ada ens [|x;y|] in
        DT.AdaBoost.adaboost_convert_prediction p
    | 'r' ->
      let data = List.map (fun (x,y) -> [|x;y|]) !pts in
      let forest =
        DT.RandomForest.train_forest
          (data, !labs) (DT.RandomForest.Ratio 0.6) 20 20
      in
      fun (x,y) -> DT.RandomForest.predict_forest forest [|x;y|]
    | _ -> fun _ -> 0
  in

  (** current predictor *)
  let predict = ref (fun _ -> 0) in

  (** main loop *)
  let rec loop () =
    clear_graph ();

    (** redraw boundary *)
    draw_decision_boundary
      world_x_min world_y_min world_width world_height
      (fun pt -> !predict pt);

    (** redraw all points *)
    List.iter2
      (fun (x,y) l -> draw_marker (x,y) l world_to_screen)
      !pts !labs;

    (** instructions *)
    draw_center_instructions
      (Printf.sprintf
         "Class=%d (press 'c' to toggle) click mouse to add press 'q' toquit"
         !current_class);

    let ev = wait_next_event [Button_down;Key_pressed] in
    if ev.key = 'q' then ()
    else if ev.key = 'c' then begin
      current_class := 1 - !current_class;
      loop ()
    end else if ev.button then begin
      (** add new point *)
      let x = world_x_min +. float ev.mouse_x *. world_width /. float window_width in
      let y = world_y_min +. float ev.mouse_y *. world_height /. float window_height in
      pts := (x,y) :: !pts;
      labs := !current_class :: !labs;
      predict := train_fn ();
      loop ()
    end else
      loop ()
  in
  loop ()

(** Main Program *)
let () =
  Random.self_init ();

  (** Open Graphics window *)
  open_graph (sprintf " %dx%d" window_width window_height);

  (** mode selection *)
  draw_center_instructions "Press 'i'=interactive, 'd'=display, 't'=Kaggle Titanic test";
  let mode = wait_for_mode () in
  if mode = 'i' then (interactive_mode (); close_graph (); exit 0)
  else if mode = 't' then (draw_center_instructions "Training random forest for Kaggle Titanic test ... this could take a minute ...";
                          kaggle_test (); close_graph (); exit 0)
  else if mode = 'd' then ()
  else (close_graph (); exit 0);

  open_graph (sprintf " %dx%d" window_width window_height);

  (** Decision Tree (2D version) *)
  set_window_title "Decision Tree2D - Graphics Interface";

  let dataset = [
    (1.0, 1.0); (2.0, 1.5); (1.5, 2.0);   (** bottom-left quadrant → class 0 *)
    (1.0, 9.0); (2.0, 8.5); (1.5, 7.5);   (** top-left quadrant    → class 1 *)
    (8.0, 1.0); (9.0, 1.5); (7.5, 2.0);   (** bottom-right         → class 1 *)
    (8.0, 9.0); (9.0, 8.0); (7.5, 7.5);   (** top-right            → class 0 *)
    (5.0, 5.0); (5.5, 5.5); (4.5, 4.5)    (** center cluster       → class 0 *)
  ] in
  let labels = [0; 0; 0; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0] in

  let tree = DT.DecisionTree2D.build_tree (dataset, labels) 0 3 in
  let test_points = [(2.5, 2.5); (7.0, 3.0); (7.5, 9.0); (0.5, 9.5); (5.0, 5.0)] in

  (** Draw decision boundary using DecisionTree2D *)
  draw_decision_boundary world_x_min world_y_min world_width world_height
    (fun (x, y) -> DT.DecisionTree2D.predict tree (x, y));
  (** Mark test points *)
  List.iter (fun pt ->
    let label = DT.DecisionTree2D.predict tree pt in
    draw_marker pt label world_to_screen;
    printf "DecisionTree2D: Point (%f, %f) => Class %d\n" (fst pt) (snd pt) label
  ) test_points;

  draw_center_instructions "Decision Tree2D: Press SPACE for next screen or Q to quit.";
  let key = wait_for_space_or_q () in
  if key = 'q' then (close_graph (); exit 0);
  set_color white;
  fill_rect ((window_width - (window_width / 2)) / 2) ((window_height - 100) / 2) (window_width / 2) 100;

  (** Decision Tree (Array version) *)
  clear_graph ();
  set_window_title "Decision Tree (Array) - Graphics Interface";

  let dataset_arr = List.map (fun (x, y) -> [| x; y |]) dataset in
  let tree_arr = DT.DecisionTree.build_tree (dataset_arr, labels) 0 3 in

  draw_decision_boundary world_x_min world_y_min world_width world_height
    (fun (x, y) -> DT.DecisionTree.predict tree_arr [| x; y |]);
  List.iter (fun (x, y) ->
    let label = DT.DecisionTree.predict tree_arr [| x; y |] in
    draw_marker (x, y) label world_to_screen;
    printf "DecisionTree (Array): Point (%f, %f) => Class %d\n" x y label
  ) test_points;
  draw_center_instructions "Decision Tree (Array): Press SPACE for next screen or Q to quit.";
  let key = wait_for_space_or_q () in
  if key = 'q' then (close_graph (); exit 0);
  set_color white;
  fill_rect ((window_width - (window_width / 2)) / 2) ((window_height - 100) / 2) (window_width / 2) 100;

   (** AdaBoost Visualization *)
  clear_graph ();
  set_window_title "AdaBoost - Graphics Interface";
  
  (* Convert dataset to array format for AdaBoost *)
  let adaboost_dataset_arr = List.map (fun (x, y) -> [| x; y |]) dataset in
  
  (** Convert labels to AdaBoost format (-1/+1) *)
  let adaboost_labels = DT.AdaBoost.adaboost_convert_labels labels in
  
  (**Train AdaBoost with 30 weak learners *)
  let adaboost_rounds = 30 in
  let adaboost_model = DT.AdaBoost.adaboost_train_ada (adaboost_dataset_arr, adaboost_labels) adaboost_rounds in
  
  printf "AdaBoost model trained with %d weak learners\n" adaboost_rounds;
  
  (** Function to predict using AdaBoost and convert back to 0/1 labels *)
  let predict_adapter (x, y) =
    let pred = DT.AdaBoost.adaboost_predict_ada adaboost_model [| x; y |] in
    DT.AdaBoost.adaboost_convert_prediction pred
  in
  
  (** Draw decision boundary using AdaBoost *)
  draw_decision_boundary world_x_min world_y_min world_width world_height predict_adapter;
  
  (** Draw test points with AdaBoost predictions *)
  List.iter (fun (x, y) ->
    let label = predict_adapter (x, y) in
    draw_marker (x, y) label world_to_screen;
    printf "AdaBoost: Point (%f, %f) => Class %d\n" x y label
  ) test_points;
  
  draw_center_instructions "AdaBoost: Press SPACE for next screen or Q to quit.";
  let key = wait_for_space_or_q () in
  if key = 'q' then (close_graph (); exit 0);
  set_color white;
  fill_rect ((window_width - (window_width / 2)) / 2) ((window_height - 100) / 2) (window_width / 2) 100;

  (** Random Forest on Spiral Data *)
  clear_graph ();
  set_window_title "Random Forest on Spiral Data - Graphics Interface";

  let generate_two_spirals n =
    let theta_max = 10.0 *. Float.pi in
    let step = theta_max /. float_of_int n in
    let spiral_a = List.init n (fun i ->
      let theta = step *. float_of_int i in
      let r = 0.5 *. theta in  
      (r *. cos theta, r *. sin theta), 0
    ) in
    let spiral_b = List.init n (fun i ->
      let theta = step *. float_of_int i +. Float.pi in
      let r = 0.5 *. theta in
      (r *. cos theta, r *. sin theta), 1
    ) in
    spiral_a @ spiral_b in
  
  let points_labels = generate_two_spirals 100 in
  let points_spiral, labels_spiral = List.split points_labels in
  let dataset_spiral = List.map (fun (x, y) -> [| x; y |]) points_spiral in
  let forest = DT.RandomForest.train_forest (dataset_spiral, labels_spiral)
                  (DT.RandomForest.Ratio 0.6) 50 30 in

  draw_decision_boundary spiral_x_min spiral_y_min spiral_width spiral_height
    (fun (x, y) -> DT.RandomForest.predict_forest forest [| x; y |]);
  List.iter (fun pt ->
    let point_arr = [| fst pt; snd pt |] in
    let label = DT.RandomForest.predict_forest forest point_arr in
    draw_marker pt label world_to_screen_spiral;
    printf "RandomForest: Point (%f, %f) => Class %d\n" (fst pt) (snd pt) label
  ) test_points;

  (** AdaBoost on Spiral Data *)
  draw_center_instructions "Random Forest on Complex Data: Press SPACE for AdaBoost on complex data or Q to quit.";
  let key = wait_for_space_or_q () in
  if key = 'q' then (close_graph (); exit 0);
  set_color white;
  fill_rect ((window_width - (window_width / 2)) / 2) ((window_height - 100) / 2) (window_width / 2) 100;
  
  clear_graph ();
  set_window_title "AdaBoost on Complex Data - Graphics Interface";

  (** Convert spiral labels to AdaBoost format *)
  let adaboost_spiral_labels = DT.AdaBoost.adaboost_convert_labels labels_spiral in
  
  (** Train AdaBoost with more weak learners for complex spiral data *)
  let adaboost_spiral_rounds = 30 in
  let adaboost_spiral_model = DT.AdaBoost.adaboost_train_ada (dataset_spiral, adaboost_spiral_labels) adaboost_spiral_rounds in
  
  (** Function to predict using AdaBoost on spiral data *)
  let predict_spiral_adapter (x, y) =
    let pred = DT.AdaBoost.adaboost_predict_ada adaboost_spiral_model [| x; y |] in
    DT.AdaBoost.adaboost_convert_prediction pred
  in
  
  (** Draw decision boundary using AdaBoost on spiral data *)
  draw_decision_boundary spiral_x_min spiral_y_min spiral_width spiral_height predict_spiral_adapter;
  
  (** Draw test points with AdaBoost predictions *)
  List.iter (fun pt ->
    let x, y = pt in
    let label = predict_spiral_adapter (x, y) in
    draw_marker pt label world_to_screen_spiral;
    printf "AdaBoost on Spiral: Point (%f, %f) => Class %d\n" x y label
  ) test_points;
  
  draw_center_instructions "AdaBoost on Spiral Data: Press SPACE to quit.";
  let key = wait_for_space_or_q () in
  if key = 'q' || key = ' ' then (close_graph (); exit 0);
  close_graph ();
  ()