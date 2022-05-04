open Core

module Make (N : Module_types.NUMBER) = struct
  module I = Interval.Make (N)
  module P = Polynomial.Make (N)
  module LE = Linear_equation.Make (N) (P)
  module QE = Quadratic_equation.Make (N) (P)
  module BS = Bisection.Make (N) (I)

  let rec roots ~eps poly =
    match P.degree poly with
    | not_nat when not_nat <= 0 -> []
    | 1 -> poly |> LE.root_poly |> Option.to_list
    | 2 -> poly |> QE.roots_poly
    | _ ->
      poly
      |> P.derivative
      |> roots ~eps:N.(eps / (one + one + one + one))
      |> I.intervals_of_list
      |> List.filter_map ~f:(BS.search ~f:(fun x -> P.calc poly ~x) ~eps)
      |> List.sort ~compare:N.ascending
  ;;
end
