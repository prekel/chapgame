open Core

module Make
    (Key : Module_types.Key)
    (Scope : Module_types.Scope)
    (N : Module_types.Number)
    (Expr : Expr.S with type key = Key.t and type scope = Scope.t and type scalar = N.t)
    (Solver : module type of Solver.MakeSolver (N)) =
    struct
  type t = (int, Expr.scalar Expr.t, Int.comparator_witness) Map.t

  let equal : t -> t -> bool = Map.equal Expr.equal

  let sexp_of_t (t : t) =
    [%sexp
      (Map.to_alist t |> List.map ~f:(fun (d, v) -> d, Expr.sexp_of_t v)
        : (int * Sexp.t) list)]
  ;;

  let t_of_sexp s =
    Map.of_alist_exn
      (module Int)
      (List.Assoc.t_of_sexp Int.t_of_sexp Expr.t_scalar_of_sexp s)
  ;;

  let of_alist_exn a = Map.of_alist_exn (module Int) a

  module Syntax = struct
    let ( + ) = Map.merge_skewed ~combine:(fun ~key:_ a b -> Expr.Sum (a, b))
    let ( ~- ) = Map.map ~f:(fun b -> Expr.Neg b)
    let ( - ) a b = a + -b

    let sqr a =
      Map.to_sequence a
      |> Sequence.cartesian_product (Map.to_sequence a)
      |> Sequence.map ~f:(fun ((d1, c1), (d2, c2)) -> Int.(d1 + d2), Expr.Mult (c1, c2))
      |> Map.of_sequence_multi (module Int)
      |> Map.map ~f:(fun a -> Expr.SumList a)
    ;;

    let scope m ~scope = Map.map m ~f:(fun v -> Expr.Scope (scope, v))
  end

  let singleton_zero a = Map.singleton (module Int) 0 a

  let to_polynomial p ~values ~scoped_values ~eps =
    Map.filter_map p ~f:(fun a ->
        match Expr.calc ~values ~scoped_values (module N) a with
        | c when N.is_finite c -> Some c
        | _ -> None)
    |> Solver.Polynomial.of_map ~eps
  ;;
end
