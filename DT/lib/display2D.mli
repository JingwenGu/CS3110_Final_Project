(** The type representing 2-dimensional mesh of points *)
type mesh = {
    x_min : float;
    y_min : float;
    x_max : float;
    y_max : float;
    x_unit : float;
    y_unit : float;
}

(** [pt_list_from_mesh_x (x,y) mesh] is the list of points in [mesh] with fixed [x] *)
val pt_list_from_mesh_x : float * 'a -> mesh -> (float * 'a) list

(** [pt_list_from_mesh_7 (x,y) mesh] is the 2D list of points in [mesh] starting from [(x,y)] *)
val pt_list_from_mesh_y : float * float -> mesh -> (float * float) list list

(** [pt_list_from_mesh_x mesh] is the list of points in [mesh] *)
val pt_list_from_mesh : mesh -> (float * float) list list

(** [plot_DT mesh f dict] prints the classified 2D space with points sampled from [mesh], classifier [f], and represented with symbol table [dict] *)
val plot_DT : mesh -> (float * float -> 'a) -> ('a -> 'b) -> unit list 

(** A mesh for demo purposes *)
val mesh_demo : mesh

(** A classifier for demo purposes *)
val f_demo : float * float -> float

(** Another classifier for demo purposes *)
val f_demo1 : float * float -> float

(** A symbol dictionary for demo purposes *)
val dict_demo : float -> unit