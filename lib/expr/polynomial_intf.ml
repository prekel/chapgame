open Core

module type S = sig
  module Var : sig
    type t
  end

  module Scope : sig
    type t
  end

  module N : Common.Module_types.NUMBER

  module Coef : sig
    type 'a t
  end

  module Polynomial : sig
    type t
  end

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
  val of_alist_exn : (int * N.t Coef.t) list -> t

  (** [singleton_zero s] if formula with single zero-degree element [s] *)
  val singleton_zero : N.t Coef.t -> t

  val to_map
    :  t
    -> values:(Var.t -> N.t)
    -> scoped_values:(Scope.t -> Var.t -> N.t)
    -> (int, N.t, Int.comparator_witness) Map.t

  (** [to_polynomial t ~values ~scoped_values ~eps] is the polynomial corresponding to the
      expression [t], with the given values for variables [values] [scoped_values], and
      precision [eps]. *)
  val to_polynomial
    :  t
    -> values:(Var.t -> N.t)
    -> scoped_values:(Scope.t -> Var.t -> N.t)
    -> eps:N.t
    -> Polynomial.t
end

module type Intf = sig
  module type S = S

  module Make
      (N : Common.Module_types.NUMBER)
      (Polynomial : Solver.Polynomial.S with module N = N)
      (Var : Module_types.VAR)
      (Scope : Module_types.SCOPE)
      (Coef : Coef.S with module Var = Var and module Scope = Scope and module N = N) :
    S
      with module Var = Var
       and module Scope = Scope
       and module N = N
       and module Coef = Coef
       and module Polynomial = Polynomial
end
