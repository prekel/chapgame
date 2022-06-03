open Core

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

  module Values : Values.S with module Var = Var

  module Rule : sig
    include
      Rule.S
        with module Var = Var
         and module Scope = Scope
         and module N = N
         and module Expr = Expr
         and module Solver = Solver
         and module Expr_polynomial = Expr_polynomial

    include Sexpable.S with type t := t
  end

  module C : sig
    val eps : N.t
  end

  module Id : Common.Module_types.IDENTIFIABLE

  type t =
    { id : Id.t
    ; values : Values.t
    ; rules : Rule.t list
    }
  [@@deriving sexp, equal]

  val calc
    :  values:(Expr.key -> Expr.scalar)
    -> rules:Rule.t list
    -> scoped_values:(Expr.scope -> Expr.key -> Expr.scalar)
    -> t:Expr.scalar
    -> ((Expr.scalar * Expr.scalar * Expr.scalar * Expr.scalar) * Rule.t list) option
end

open Core

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
                            and module Solver = Solver)
      (Values : Values.S with module Var = Var and module Scope = Scope and module N = N)
      (Rule : sig
        include
          Rule.S
            with module Var = Var
             and module Scope = Scope
             and module N = N
             and module Expr = Expr
             and module Solver = Solver
             and module Expr_polynomial = Expr_polynomial

        include Sexpable.S with type t := t
      end) (C : sig
        val eps : N.t
      end) :
    S
      with module Var = Var
       and module Scope = Scope
       and module N = N
       and module Expr = Expr
       and module Solver = Solver
       and module Expr_polynomial = Expr_polynomial
       and module Values = Values
       and module Rule = Rule
       and module C = C
end
