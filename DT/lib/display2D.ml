type mesh = {
  x_min : float;
  y_min : float;
  x_max : float;
  y_max : float;
  x_unit : float;
  y_unit : float;
}

let rec pt_list_from_mesh_x (x,y) mesh = 
  if x > mesh.x_max then []
  else (x,y) :: (pt_list_from_mesh_x (x+.mesh.x_unit, y) mesh)

let rec pt_list_from_mesh_y (x,y) mesh = 
  if y < mesh.y_min then []
  else (pt_list_from_mesh_x (x,y) mesh) :: (pt_list_from_mesh_y (x, y-.mesh.y_unit) mesh)

let pt_list_from_mesh mesh = pt_list_from_mesh_y (mesh.x_min,mesh.y_max) mesh
  
let print_symb_list symb_list = 
  let list_f = fun lst -> List.fold_left ( ^ ) "" lst in
  List.map (fun lst -> lst |> list_f |> print_endline) symb_list

let plot_DT mesh f dict =
  let pt_f = fun (x,y) -> (x,y) |> f |> dict in
  let list_f = fun lst -> List.map pt_f lst in
  let list2d_f = fun lst -> List.map list_f lst in
  mesh |> pt_list_from_mesh |> list2d_f |> print_symb_list

let mesh_demo = { x_min = -10.0; y_min = -10.0; x_max = 10.0; y_max = 10.0; x_unit = 0.5; y_unit = 0.5 };;

let f_demo (x,y) = y -. x*.x

let f_demo1 (x,y) = (x*.x+.y*.y)/.(x+.y)-.4.0

let dict_demo x = if x > 0.0 then "##" else "__"