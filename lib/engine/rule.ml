open Core

module Make
    (Var : Module_types.VAR)
    (Scope : Module_types.SCOPE)
    (N : Solver.Module_types.NUMBER)
    (Expr : module type of Expr.Make (Var) (Scope) (N))
    (Solver : module type of Solver.All.Make (N))
    (Formula : module type of Formula.Make (Var) (Scope) (N) (Expr) (Solver)) =
    struct
      type t =
        { interval :
            [ `Interval of Expr.t_scalar * Expr.t_scalar | `PosInfinity of Expr.t_scalar ]
        ; x : Formula.t
        ; y : Formula.t
        ; v_x : Formula.t
        ; v_y : Formula.t
        ; after : t list
        ; name : string
        }
      [@@deriving equal]
    end
