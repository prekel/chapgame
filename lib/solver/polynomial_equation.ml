open Core

module Make
    (N : Module_types.NUMBER)
    (I : module type of Interval.Make (N))
    (P : module type of Polynomial.Make (N))
    (LE : module type of Linear_equation.Make (N) (P))
    (QE : module type of Quadratic_equation.Make (N) (P))
    (BS : module type of Bisection.Make (N) (I)) =
    struct
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
