open Core

(* module type S = *)

module type Intf = sig
  (* module type S = S *)

  module Make
      (N : Solver.Module_types.NUMBER)
      (Var : Module_types.VAR)
      (Scope : Module_types.SCOPE) : sig
    module P : Solver.Polynomial.S with module N = N
    module PE : Solver.Polynomial_equation.S with module N = N and module Polynomial = P
    module V : Values.S with module N = N and module Var = Var and module Scope = Scope
    module E : Expr.S with module Var = Var and module Scope = Scope and module N = N

    module EP :
      Expr_polynomial.S
        with module Var = Var
         and module Scope = Scope
         and module N = N
         and module Expr = E
         and module Polynomial = P

    module R :
      Rule.S
        with module Var = Var
         and module Scope = Scope
         and module N = N
         and module Expr = E
         and module Expr_polynomial = EP

    module B (Rule : sig
      include module type of R
      include Sexpable.S with type t := t
    end) (C : sig
      val eps : N.t
    end) :
      Body.S
        with module Var = Var
         and module Scope = Scope
         and module N = N
         and module Values = V
         and module Rule = Rule

    module Pt : Point.S with module N = N
    module Ln : Line.S with module N = N and module Point = Pt
  end
end
