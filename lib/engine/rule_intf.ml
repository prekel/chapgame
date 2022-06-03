module type S = sig
  module Var : Module_types.VAR
  module Scope : Module_types.SCOPE
  module N : Solver.Module_types.NUMBER
  module Expr : Expr.S with module Var = Var and module Scope = Scope and module N = N
  module Solver : Solver.All.S with module N = N

  module Expr_polynomial :
    Expr_polynomial.S
      with module Var = Var
       and module Scope = Scope
       and module N = N
       and module Expr = Expr
       and module Solver = Solver

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

module type Intf = sig
  module type S = S

  module Make
      (Var : Module_types.VAR)
      (Scope : Module_types.SCOPE)
      (N : Solver.Module_types.NUMBER)
      (Expr : Expr.S with module Var = Var and module Scope = Scope and module N = N)
      (Solver : Solver.All.S with module N = N)
      (Expr_polynomial : Expr_polynomial.S
                           with module Var = Var
                            and module Scope = Scope
                            and module N = N
                            and module Expr = Expr
                            and module Solver = Solver) :
    S
      with module Var = Var
       and module Scope = Scope
       and module N = N
       and module Expr = Expr
       and module Solver = Solver
       and module Expr_polynomial = Expr_polynomial
end
