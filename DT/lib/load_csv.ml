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
     - Categorical: fill missing with majority value, then map to random int codes *)
  let convert_dataset (ds : dataset)
    : bool array * (string, int) Hashtbl.t array * (string * float array) list =
    Random.self_init ();
    let m = Array.length ds.features in

    (* 1) Detect numeric columns (ignore missing="") *)
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

    (* 2) Compute defaults: mean for numeric, majority for categorical *)
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

    (* 3) Build maps and convert rows to numeric arrays *)
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
                  let code =
                    match Hashtbl.find_opt tbl vstr with
                    | Some c -> c
                    | None ->
                      let rec fresh () =
                        let c0 = Random.int 1_000_000 in
                        if Hashtbl.fold (fun _ c seen -> seen || c = c0) tbl false
                        then fresh ()
                        else c0
                      in
                      let c1 = fresh () in
                      Hashtbl.add tbl vstr c1;
                      c1
                  in
                  float_of_int code)
              values
          in
          (id, arr))
        ds.rows
    in
    (is_numeric, maps, data)
end
