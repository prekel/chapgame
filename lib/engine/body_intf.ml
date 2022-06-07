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

  module Values : sig
    type t
  end

  module Rule : sig
    type t
  end

  module Id : Common.Module_types.IDENTIFIABLE

  type t =
    { id : Id.t
    ; values : Values.t
    ; rules : Rule.t list
    }
  [@@deriving sexp, equal]

  val calc
    :  values:(Var.t -> N.t)
    -> rules:Rule.t list
    -> scoped_values:(Scope.t -> Var.t -> N.t)
    -> t:N.t
    -> ((N.t * N.t * N.t * N.t) * Rule.t list) option
end

open Core

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
                            and module Polynomial = Polynomial)
      (Values : Values.S with module Var = Var and module Scope = Scope and module N = N)
      (Rule : sig
        include
          Rule.S
            with module Var = Var
             and module Scope = Scope
             and module N = N
             and module Expr = Expr
             and module Expr_polynomial = Expr_polynomial

        include Sexpable.S with type t := t
      end) (C : sig
        val eps : N.t
      end) :
    S
      with module Var = Var
       and module Scope = Scope
       and module N = N
       and module Values = Values
       and module Rule = Rule
end
