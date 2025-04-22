(* module type Dataset = sig
  type x

  type y

  type t

  val create : x list -> y list -> t

  val points : t -> x list

  val labels : t -> y list
end

module Dataset_f2i1 = struct
  type x = float * float

  type y = int

  type t = x list * y list

  let create (points : x list) (labels : y list) = (points,labels)

  let points (ds : t) = fst ds

  let labels (ds : t) = snd ds
end *)