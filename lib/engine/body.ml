open Core
include Body_intf

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
    end) =
struct
  module Var = Var
  module Scope = Scope
  module N = N
  module Expr = Expr
  module Solver = Solver
  module Expr_polynomial = Expr_polynomial
  module Values = Values
  module Rule = Rule
  module C = C

  module Id = Common.Utils.MakeIntId (struct
    let module_name = "Body.Id"
  end)

  type t =
    { id : Id.t
    ; values : Values.t
    ; rules : Rule.t list
    }
  [@@deriving sexp, equal]

  let calc ~values ~rules ~scoped_values ~t =
    let c = Expr.calc ~values ~scoped_values (module N) in
    let calc_xy f =
      Expr_polynomial.to_polynomial f ~values ~scoped_values ~eps:C.eps
      |> Polynomial.calc ~x:t
    in
    List.find_map rules ~f:(fun Rule.{ interval; x; y; v_x; v_y; after; _ } ->
        match interval with
        | `Interval (l, r) when N.(c l <= t && t < c r) ->
          Some ((calc_xy x, calc_xy y, calc_xy v_x, calc_xy v_y), after)
        | `PosInfinity l when N.(c l <= t) ->
          Some ((calc_xy x, calc_xy y, calc_xy v_x, calc_xy v_y), after)
        | _ -> None)
  ;;
end
