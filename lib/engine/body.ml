open Core
open Open

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
  let c = ExprCoef.calc ~values ~scoped_values (module N) in
  let calc_xy f =
    Formula.to_polynomial f ~values ~scoped_values ~eps |> Solver.P.calc ~x:t
  in
  List.find_map rules ~f:(fun Rule.{ interval; x; y; v_x; v_y; after; _ } ->
      match interval with
      | `Interval (l, r) when N.(c l <= t && t < c r) ->
        Some ((calc_xy x, calc_xy y, calc_xy v_x, calc_xy v_y), after)
      | `PosInfinity l when N.(c l <= t) ->
        Some ((calc_xy x, calc_xy y, calc_xy v_x, calc_xy v_y), after)
      | _ -> None)
;;

let update_x0y0 ~body (x, y, v_x, v_y) ~rules =
  { body with
    values =
      body.values
      |> Values.update_scalar ~var:`x0 ~value:x
      |> Values.update_scalar ~var:`y0 ~value:y
      |> Values.update_vector ~var_x:`v0_x ~var_y:`v0_y ~value:(v_x, v_y)
  ; rules
  }
;;

let update_v0 body ~v ~rules =
  { body with
    values = body.values |> Values.update_vector ~var_x:`v0_x ~var_y:`v0_y ~value:v
  ; rules
  }
;;

let get_id { id; _ } = id
let get_values { values; _ } = values

let calc_a ~global_values { values; _ } =
  let calc =
    ExprCoef.calc
      ~values:(Values.to_function values)
      ~scoped_values:(Values.global_to_scoped global_values)
      (module Vector)
  in
  calc Rule.Exprs.a_vec
;;
