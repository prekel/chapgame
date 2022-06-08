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
end

module type Intf = sig
  module type S = S

  module Make
      (N : Common.Module_types.NUMBER)
      (Var : Module_types.VAR)
      (Scope : Module_types.SCOPE)
      (Coef : Coef.S with module Var = Var and module Scope = Scope and module N = N) :
    S
      with module Var = Var
       and module Scope = Scope
       and module N = N
       and module Coef = Coef
end
