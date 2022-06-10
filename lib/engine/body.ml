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
