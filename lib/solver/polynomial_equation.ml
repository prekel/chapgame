open Core
include Polynomial_equation_intf

module Make
    (N : Common.Module_types.NUMBER)
    (I : Interval.S with module N = N)
    (Polynomial : Polynomial.S with module N = N)
    (BS : Bisection.S with module N = N and module Interval = I) =
struct
  module N = N
  module Polynomial = Polynomial

  let two = N.(one + one)
  let four = N.(two + two)

  let linear_root poly =
    let root ~a ~b = N.(-b / a) in
    let poly = Polynomial.to_map poly in
    match Map.find poly 1, Map.find poly 0 with
    | Some a, Some b -> Some (root ~a ~b)
    | Some a, None -> Some (root ~a ~b:N.zero)
    | None, _ -> None
  ;;

  let quadratic_roots poly ~eps =
    let roots ~a ~b ~c ~eps =
      let open N in
      match square b - (four * a * c) with
      | d when abs d <= eps -> [ -b / (two * a) ]
      | d when d > eps ->
        let x1 = (-b - sqrt d) / (two * a) in
        let x2 = (-b + sqrt d) / (two * a) in
        [ min x1 x2; max x1 x2 ]
      | _ -> []
    in
    let poly = Polynomial.to_map poly in
    match Map.find poly 2, Map.find poly 1, Map.find poly 0 with
    | Some a, Some b, Some c -> roots ~a ~b ~c ~eps
    | Some a, None, Some c -> roots ~a ~b:N.zero ~c ~eps
    | Some a, Some b, None -> roots ~a ~b ~c:N.zero ~eps
    | Some a, None, None -> roots ~a ~b:N.zero ~c:N.zero ~eps
    | None, _, _ -> []
  ;;

  let rec roots ~eps poly =
    match Polynomial.degree poly with
    | non_nat when non_nat <= 0 -> []
    | 1 -> poly |> linear_root |> Option.to_list
    | 2 -> poly |> quadratic_roots ~eps
    | _ ->
      poly
      |> Polynomial.derivative
      |> roots ~eps
      |> I.intervals_of_list
      |> List.filter_map ~f:(BS.search ~f:(fun x -> Polynomial.calc poly ~x) ~eps)
      |> List.sort ~compare:N.ascending
  ;;
end
