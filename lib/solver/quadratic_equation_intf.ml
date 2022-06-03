module type S = sig
  module N : sig
    type t
  end

  module Polynomial : sig
    type t
  end

  val roots : a:N.t -> b:N.t -> c:N.t -> eps:N.t -> N.t list
  val roots_poly : Polynomial.t -> eps:N.t -> N.t list
end

module type Intf = sig
  module type S = S

  module Make (N : Module_types.NUMBER) (Polynomial : Polynomial.S with module N = N) :
    S with module N = N and module Polynomial = Polynomial
end
