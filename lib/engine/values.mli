open Open

type t [@@deriving sexp, equal]

val get_scalar_exn : t -> var:Var.t -> N.t
val get_vector_exn : t -> var_x:Var.t -> var_y:Var.t -> N.t * N.t
val update_scalar : t -> var:Var.t -> value:N.t -> t
val update_vector : t -> var_x:Var.t -> var_y:Var.t -> value:N.t * N.t -> t
val of_alist : (Var.t * N.t) list -> t
val to_function : t -> Var.t -> N.t
val global_to_scoped : t -> Scope.t -> Var.t -> N.t

module Diff :
  Common.Utils.AdvancedMapDiff with type tt = t and type key = Var.t and type value = N.t
