open Core

module Make
    (Var : Module_types.VAR)
    (Scope : Module_types.SCOPE)
    (N : Solver.Module_types.NUMBER)
    (Expr : module type of Expr.Make (Var) (Scope) (N))
    (Solver : module type of Solver.All.Make (N))
    (Expr_polynomial : module type of Expr_polynomial.Make (Var) (Scope) (N) (Expr) (Solver))
    (Values : module type of Values.Make (Var) (Scope) (N)) (Rule : sig
      include module type of Rule.Make (Var) (Scope) (N) (Expr) (Solver) (Expr_polynomial)
      include Sexpable.S with type t := t
    end) (C : sig
      val eps : N.t
    end) =
    struct
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
          Expr_polynomial.to_polynomial f ~values ~scoped_values ~eps:C.eps |> Solver.P.calc ~x:t
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
