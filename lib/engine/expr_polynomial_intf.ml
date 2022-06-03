module type S = sig
  module Var : Module_types.VAR
  module Scope : Module_types.SCOPE
  module N : Solver.Module_types.NUMBER
  module Expr : Expr.S with module Var = Var and module Scope = Scope and module N = N
  module Solver : Solver.All.S with module N = N

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
    -> values:(Var.t -> N.t)
    -> scoped_values:(Scope.t -> Var.t -> N.t)
    -> eps:N.t
    -> Solver.P.t
end

module type Intf = sig
  module type S = S

  module Make
      (Var : Module_types.VAR)
      (Scope : Module_types.SCOPE)
      (N : Solver.Module_types.NUMBER)
      (Expr : Expr.S with module Var = Var and module Scope = Scope and module N = N)
      (Solver : Solver.All.S with module N = N) :
    S
      with module Var = Var
       and module Scope = Scope
       and module N = N
       and module Expr = Expr
       and module Solver = Solver
end
