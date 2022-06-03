module type S = sig
  module N : sig
    type t
  end

  module Polynomial : sig
    type t
  end

  val root : a:N.t -> b:N.t -> N.t
  val root_opt : a:N.t -> b:N.t -> N.t option
  val root_poly : Polynomial.t -> N.t option
end

module type Intf = sig
  module type S = S

  module Make (N : Module_types.NUMBER) (Polynomial : Polynomial.S with module N = N) :
    S with module N = N and module Polynomial = Polynomial
end
