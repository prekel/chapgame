open Core

include Linear_equation_intf

module Make
    (N : Module_types.NUMBER)
    (Polynomial : module type of Polynomial.Make (N)) =
    struct
  let root ~a ~b = N.(-b / a)
  let root_opt ~a ~b = if N.(a = zero) then None else Some N.(-b / a)

  let root_poly poly =
    let poly = Polynomial.to_map poly in
    match Map.find poly 1, Map.find poly 0 with
    | Some a, Some b -> Some (root ~a ~b)
    | Some a, None -> Some (root ~a ~b:N.zero)
    | None, _ -> None
  ;;
end
