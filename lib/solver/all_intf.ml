module type S = sig
  module N : Module_types.NUMBER
  module I : Interval.S with module N = N
  module P : Polynomial.S with module N = N
  module LE : Linear_equation.S with module N = N
  module QE : Quadratic_equation.S with module N = N and module Polynomial = P
  module BS : Bisection.S with module N = N and module Interval = I
  module PE : Polynomial_equation.S with module N = N and module P = P
end

module type Ints = sig
  module type S = S

  module Make (N : Module_types.NUMBER) : S with module N = N
end
