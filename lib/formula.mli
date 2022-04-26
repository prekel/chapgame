module Make
    (Key : Module_types.KEY)
    (Scope : Module_types.SCOPE)
    (N : Module_types.NUMBER)
    (Expr : Expr.S with type key = Key.t and type scope = Scope.t and type scalar = N.t)
    (Solver : module type of Solver.MakeSolver (N)) : sig
  type t [@@deriving sexp, equal]

  module Syntax : sig
    val ( + ) : t -> t -> t
    val ( ~- ) : t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> t -> t
    val sqr : t -> t
    val scope : t -> scope:Scope.t -> t
  end

  (** [of_alist_exn alist] is formula made of associative list [alist] *)
  val of_alist_exn : (int * Expr.scalar Expr.t) list -> t

  (** [singleton_zero s] if formula with single zero-degree element [s] *)
  val singleton_zero : Expr.scalar Expr.t -> t

  (** [to_polynomial t ~values ~scoped_values ~eps] is the polynomial corresponding to the
      expression [t], with the given values for variables [values] [scoped_values], and
      precision [eps]. *)
  val to_polynomial
    :  t
    -> values:(Key.t -> N.t)
    -> scoped_values:(Scope.t -> Key.t -> N.t)
    -> eps:N.t
    -> Solver.Polynomial.t
end
