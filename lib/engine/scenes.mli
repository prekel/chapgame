open Core 
type t [@@deriving sexp, equal]

  val before : t -> time:float -> t * Scene.t
  val merge_with_list : t -> Scene.t list -> t
  val last_exn : t -> Scene.t
  val to_map : t -> (float, Scene.t, Float.comparator_witness) Map.t
  val of_map : (float, Scene.t, Float.comparator_witness) Map.t -> t
  val to_sequence : t -> (float * Scene.t) Sequence.t
  val get_by_id : t -> id:float -> Scene.t
  