open Core
module N = Float

let eps = 1e-6

module Var = struct
  type t =
    [ `x0
    | `y0
    | `v0_x
    | `v0_y
    | `r
    | `g
    | `mu
    | `m
    | `with_point of [ `x | `y ]
    | `with_line of [ `a | `b | `c | `a2b2 ]
    ]
  [@@deriving sexp, equal, compare]

  include Comparable.Make (struct
    type nonrec t = t [@@deriving sexp, equal, compare]
  end)

  let x0 = `x0
  let y0 = `y0
  let v0_x = `v0_x
  let v0_y = `v0_y
  let r = `r
  let g = `g
  let mu = `mu
  let m = `m
end

module Vars = Var

module Scope = struct
  type t =
    [ `Global
    | `_1
    | `_2
    ]
  [@@deriving sexp, equal]

  let is_global = equal `Global
end

module Solver = struct
  module P = Solver.Polynomial.Make (Float)
  module I = Solver.Interval.Make (Float)

  module PE =
    Solver.Polynomial_equation.Make (Float) (I) (P) (Solver.Bisection.Make (Float) (I))
end

module ExprCoef = Expr.Coef.Make (N) (Var) (Scope)
module Formula = Expr.Polynomial.Make (N) (Solver.P) (Var) (Scope) (ExprCoef)
module Points = Common.Utils.MakeAdvancedSet (Point)
module Lines = Common.Utils.MakeAdvancedSet (Line)
module Vector = Common.Vector.Make (N)
