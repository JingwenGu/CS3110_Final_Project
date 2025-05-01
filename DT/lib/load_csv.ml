module Data = struct
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
                  "Bad row: expected %d fields but got %d"
                  (Array.length header)
                  (Array.length fields));
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

  (** Convert raw data to either Train (features * labels) or Test features list.
      [label_column] is the index of the label within each feature array,
      or [-1] for a test set (no labels).
  *)
  let prepare_for_tree ~label_column data =
    if label_column < 0 then
      Test (List.map snd data)
    else if label_column >= 0 then
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
    else
      failwith "Invalid label_column"
end