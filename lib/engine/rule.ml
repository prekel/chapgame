open Core

module Make
    (Var : Module_types.VAR)
    (Scope : Module_types.SCOPE)
    (N : Solver.Module_types.NUMBER)
    (Expr : module type of Expr.Make (Var) (Scope) (N))
    (Solver : module type of Solver.All.Make (N))
    (Expr_polynomial : module type of Expr_polynomial.Make (Var) (Scope) (N) (Expr) (Solver)) =
    struct
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
