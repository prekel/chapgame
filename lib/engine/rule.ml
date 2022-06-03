open Core
include Rule_intf

module Make
    (Var : Module_types.VAR)
    (Scope : Module_types.SCOPE)
    (N : Solver.Module_types.NUMBER)
    (Expr : Expr.S with module Var = Var and module Scope = Scope and module N = N)
    (Polynomial : Solver.Polynomial.S with module N = N)
    (Expr_polynomial : Expr_polynomial.S
                         with module Var = Var
                          and module Scope = Scope
                          and module N = N
                          and module Expr = Expr
                          and module Polynomial = Polynomial) =
struct
  module Var = Var
  module Scope = Scope
  module N = N
  module Expr = Expr
  module Expr_polynomial = Expr_polynomial

  type t =
    { interval :
        [ `Interval of Expr.t_scalar * Expr.t_scalar | `PosInfinity of Expr.t_scalar ]
    ; x : Expr_polynomial.t
    ; y : Expr_polynomial.t
    ; v_x : Expr_polynomial.t
    ; v_y : Expr_polynomial.t
    ; after : t list
    ; name : string
    }
  [@@deriving equal]
end
