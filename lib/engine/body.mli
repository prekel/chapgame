module Id : Common.Module_types.IDENTIFIABLE

type t =
  { id : Id.t
  ; values : Values.t
  ; rules : Rule.t list
  }

val t_of_sexp : Sexplib0.Sexp.t -> t
val sexp_of_t : t -> Sexplib0.Sexp.t
val equal : t -> t -> bool

val calc
  :  values:(Open.Var.t -> float)
  -> rules:Rule.t list
  -> scoped_values:(Open.Scope.t -> Open.Var.t -> float)
  -> t:float
  -> ((float * float * float * float) * Rule.t list) option

val update_x0y0 : body:t -> float * float * float * float -> rules:Rule.t list -> t
val update_v0 : t -> v:float * float -> rules:Rule.t list -> t
val get_id : t -> Id.t
val get_values : t -> Values.t
val calc_a : global_values:Values.t -> t -> Open.Vector.t
