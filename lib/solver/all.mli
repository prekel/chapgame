module Make (N :Module_types.NUMBER) : sig
  module I : module type of Interval.Make (N)
  module P : module type of Polynomial.Make (N)
  module LE : module type of Linear_equation.Make (N) (P)
  module QE : module type of Quadratic_equation.Make (N) (P)
  module BS : module type of Bisection.Make (N) (I)
  module PE : module type of Polynomial_equation.Make (N) (I) (P) (LE) (QE) (BS)
end
