open Core

module Make
    (Var : Module_types.VAR)
    (Scope : Module_types.SCOPE)
    (N : Solver.Module_types.NUMBER) =
struct
  include Common.Utils.MakeAdvancedMap (Var) (N)

  let get_scalar_exn values ~var = Map.find_exn values var

  let get_vector_exn values ~var_x ~var_y =
    let x = Map.find_exn values var_x in
    let y = Map.find_exn values var_y in
    x, y
  ;;

  let update_scalar values ~var ~value = Map.update values var ~f:(fun _ -> value)

  let update_vector values ~var_x ~var_y ~value =
    let wx = Map.update values var_x ~f:(fun _ -> fst value) in
    let wy = Map.update wx var_y ~f:(fun _ -> snd value) in
    wy
  ;;

  let of_alist = of_alist_exn
  let to_function = find_exn
  let global_to_scoped g s = if Scope.is_global s then to_function g else assert false
end