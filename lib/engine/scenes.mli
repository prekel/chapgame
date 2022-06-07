open Core 

open Open

type t [@@deriving sexp, equal]

  val before : t -> time:N.t -> t * Scene.t
  val merge_with_list : t -> Scene.t list -> t
  val last_exn : t -> Scene.t
  val to_map : t -> (N.t, Scene.t, N.comparator_witness) Map.t
  val of_map : (N.t, Scene.t, N.comparator_witness) Map.t -> t
  val to_sequence : t -> (N.t * Scene.t) Sequence.t
  val get_by_id : t -> id:N.t -> Scene.t
  