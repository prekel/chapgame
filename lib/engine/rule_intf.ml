module type S = sig
  module Var : sig
    type t
  end

  module Scope : sig
    type t
  end

  module N : sig
    type t
  end

  module Expr : sig
    type t_scalar
  end

  module Expr_polynomial : sig
    type t
  end

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
      (N : Solver.Module_types.NUMBER)
      (Polynomial : Solver.Polynomial.S with module N = N)
      (Var : Module_types.VAR)
      (Scope : Module_types.SCOPE)
      (Expr : Expr.S with module Var = Var and module Scope = Scope and module N = N)
      (Expr_polynomial : Expr_polynomial.S
                           with module Var = Var
                            and module Scope = Scope
                            and module N = N
                            and module Expr = Expr
                            and module Polynomial = Polynomial) :
    S
      with module Var = Var
       and module Scope = Scope
       and module N = N
       and module Expr = Expr
       and module Expr_polynomial = Expr_polynomial
end
