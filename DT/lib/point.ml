(* module type Point = sig
  type t

  exception Invalid_point_args

  val make : 'a -> t
end

module Point2D : Point = struct
  type t = float * float

  exception Invalid_point_args

  let make p = match p with
    | (x,y) -> try (x +. 0.0, y +. 0.0)
              with raise Invalid_point_args
    | _ -> raise Invalid_point_args
end

module PointArr : Point = struct
  type t = float array
end *)