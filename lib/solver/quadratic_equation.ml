open Core

module Make
    (N : Module_types.NUMBER)
    (Polynomial : module type of Polynomial.Make (N)) =
    struct
  let two = N.(one + one)
  let four = N.(two + two)

  let roots ~a ~b ~c ~eps =
    let open N in
    match square b - (four * a * c) with
    | d when abs d <= eps -> [ -b / (two * a) ]
    | d when d > eps ->
      let x1 = (-b - sqrt d) / (two * a) in
      let x2 = (-b + sqrt d) / (two * a) in
      [ min x1 x2; max x1 x2 ]
    | _ -> []
  ;;

  let roots_poly poly ~eps =
    let poly = Polynomial.to_map poly in
    match Map.find poly 2, Map.find poly 1, Map.find poly 0 with
    | Some a, Some b, Some c -> roots ~a ~b ~c ~eps
    | Some a, None, Some c -> roots ~a ~b:N.zero ~c ~eps
    | Some a, Some b, None -> roots ~a ~b ~c:N.zero ~eps
    | Some a, None, None -> roots ~a ~b:N.zero ~c:N.zero ~eps
    | None, _, _ -> []
  ;;
end
