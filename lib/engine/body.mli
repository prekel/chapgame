module Id : Common.Module_types.IDENTIFIABLE

type t =
  { id : Id.t
  ; values : Values.t
  ; rules : Rule.t list
  }
[@@deriving sexp, equal]

val update_x0y0 : body:t -> float * float * float * float -> rules:Rule.t list -> t
val update_v0 : t -> v:float * float -> rules:Rule.t list -> t
val get_id : t -> Id.t
val get_values : t -> Values.t
val calc_a : global_values:Values.t -> t -> Open.Vector.t
