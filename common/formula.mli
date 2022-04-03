module Make
    (Key : Module_types.Key)
    (Scope : Module_types.Scope)
    (N : Module_types.Number)
    (Expr : Expr.S with type key = Key.t and type scope = Scope.t and type scalar = N.t)
    (Solver : module type of Solver.MakeSolver (N)) : sig
  type t [@@deriving sexp, equal]

  val of_alist_exn : (int * Expr.scalar Expr.t) list -> t

  module Syntax : sig
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val sqr : t -> t
    val scope : t -> scope:Scope.t -> t
  end

  val to_polynomial
    :  t
    -> values:Expr.values
    -> scoped_values:(Scope.t -> Expr.values)
    -> eps:N.t
    -> Solver.Polynomial.t
end
