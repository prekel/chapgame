open Core

module Make (N : Module_types.NUMBER) = struct
  type t = (int, N.t, Int.comparator_witness) Map.t

  let equal = Map.equal N.equal
  let normalize ~eps = Map.filter ~f:N.(fun coef -> N.is_finite coef && abs coef > eps)
  let of_list a ~eps = Map.of_alist_exn (module Int) a |> normalize ~eps
  let of_list_raw a = Map.of_alist_exn (module Int) a
  let t_of_sexp s = List.Assoc.t_of_sexp Int.t_of_sexp N.t_of_sexp s |> of_list_raw
  let sexp_of_t t = [%sexp (Map.to_alist t : (int * N.t) list)]

  let derivative p =
    Map.filter_mapi p ~f:(fun ~key:degree ~data:coefficient ->
        match degree with
        | 0 -> None
        | _ -> Some N.(coefficient * of_int degree))
    |> Map.map_keys_exn (module Int) ~f:(fun a -> a - 1)
  ;;

  let calc poly ~x =
    Map.fold poly ~init:N.zero ~f:(fun ~key:degree ~data:coef acc ->
        N.(acc + (coef * (x ** of_int degree))))
  ;;

  let degree p =
    match Map.max_elt p with
    | Some (deg, _) -> deg
    | None -> 0
  ;;

  let to_map = Fn.id
  let of_map m ~eps = normalize m ~eps

  let to_string_hum ?var =
    let var = Option.value ~default:"x" var in
    Map.fold_right ~init:"" ~f:(fun ~key:degree ~data:coef acc ->
        acc
        ^ N.to_string_hum ~strip_zero:true ~explicit_plus:String.(acc <> "") coef
        ^ var
        ^ "^"
        ^ Int.to_string degree)
  ;;
end
