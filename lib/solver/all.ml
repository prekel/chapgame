include All_intf

module Make (N : Module_types.NUMBER) = struct
  module I = Interval.Make (N)
  module P = Polynomial.Make (N)
  module LE = Linear_equation.Make (N) (P)
  module QE = Quadratic_equation.Make (N) (P)
  module BS = Bisection.Make (N) (I)
  module PE = Polynomial_equation.Make (N) (I) (P) (LE) (QE) (BS)
end
