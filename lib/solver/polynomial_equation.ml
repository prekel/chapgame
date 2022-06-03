open Core
include Polynomial_equation_intf

module Make
    (N : Module_types.NUMBER)
    (I : Interval.S with module N = N)
    (P : Polynomial.S with module N = N)
    (LE : Linear_equation.S with module N = N and module Polynomial = P)
    (QE : Quadratic_equation.S with module N = N and module Polynomial = P)
    (BS : Bisection.S with module N = N and module Interval = I) =
struct
  module N = N
  module P = P

  let rec roots ~eps poly =
    match P.degree poly with
    | non_nat when non_nat <= 0 -> []
    | 1 -> poly |> LE.root_poly |> Option.to_list
    | 2 -> poly |> QE.roots_poly ~eps
    | _ ->
      poly
      |> P.derivative
      |> roots ~eps:N.(eps / (one + one + one + one))
      |> I.intervals_of_list
      |> List.filter_map ~f:(BS.search ~f:(fun x -> P.calc poly ~x) ~eps)
      |> List.sort ~compare:N.ascending
  ;;
end
