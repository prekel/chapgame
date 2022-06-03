open Core

module type S = sig
  module N : Module_types.NUMBER
  module P : Polynomial.S with module N = N

  (** [roots ~eps poly] is roots of equation [poly] = 0 when [abs (calc poly ~x) < eps]*)
  val roots : eps:N.t -> P.t -> N.t list
end

module type Intf = sig
  module type S = S

  module Make
      (N : Module_types.NUMBER)
      (I : Interval.S with module N = N)
      (P : Polynomial.S with module N = N)
      (LE : Linear_equation.S with module N = N and module Polynomial = Polynomial)
      (QE : Quadratic_equation.S with module N = N and module Polynomial = P)
      (BS : Bisection.S with module N = N and module Interval = I) : S with module N = N
end