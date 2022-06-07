include Deps_intf

module Make
    (N : Solver.Module_types.NUMBER)
    (Var : Module_types.VAR)
    (Scope : Module_types.SCOPE) =
struct
  module P = Solver.Polynomial.Make (N)
  module I = Solver.Interval.Make (N)
  module PE = Solver.Polynomial_equation.Make (N) (I) (P) (Solver.Bisection.Make (N) (I))
  module V = Values.Make (N) (Var) (Scope)
  module E = Expr.Make (N) (Var) (Scope)
  module EP = Expr_polynomial.Make (N) (P) (Var) (Scope) (E)
  module R = Rule.Make (N) (P) (Var) (Scope) (E) (EP)
  module B = Body.Make (N) (P) (Var) (Scope) (E) (EP) (V)
  module Pt = Point.Make (N)
  module Ln = Line.Make (N) (Pt)
  module N = N
  module Var = Var
  module Scope = Scope
end
