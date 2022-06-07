open Open

type t [@@deriving sexp, equal]

val get_scalar_exn : t -> var:Var.t -> float
val get_vector_exn : t -> var_x:Var.t -> var_y:Var.t -> float * float
val update_scalar : t -> var:Var.t -> value:float -> t
val update_vector : t -> var_x:Var.t -> var_y:Var.t -> value:float * float -> t
val of_alist : (Var.t * float) list -> t
val to_function : t -> Var.t -> float
val global_to_scoped : t -> Scope.t -> Var.t -> float

module Diff :
  Common.Utils.AdvancedMapDiff
    with type tt = t
     and type key = Var.t
     and type value = float
