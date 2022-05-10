open Core

module Make
    (Key : Module_types.VAR)
    (Scope : Module_types.SCOPE)
    (N : Solver.Module_types.NUMBER)
    (Expr : Expr.S with type key = Key.t and type scope = Scope.t and type scalar = N.t)
    (Solver : module type of Solver.All.Make (N)) =
    struct
  include
    Common.Utils.MakeAdvancedMap
      (Int)
      (struct
        type t = Expr.scalar Expr.t

        let equal = Expr.equal_t_scalar
        let t_of_sexp = Expr.t_scalar_of_sexp
        let sexp_of_t = Expr.sexp_of_t
      end)

  module Syntax = struct
    let ( + ) = Map.merge_skewed ~combine:(fun ~key:_ a b -> Expr.Sum (a, b))
    let ( ~- ) = Map.map ~f:(fun b -> Expr.Neg b)
    let ( - ) a b = a + -b

    let ( * ) a b =
      Sequence.cartesian_product (Map.to_sequence a) (Map.to_sequence b)
      |> Sequence.map ~f:(fun ((d1, c1), (d2, c2)) -> Int.(d1 + d2), Expr.Mult (c1, c2))
      |> Map.of_sequence_multi (module Int)
      |> Map.map ~f:(fun a -> Expr.SumList a)
    ;;

    let sqr a = a * a
    let scope m ~scope = Map.map m ~f:(fun v -> Expr.Scope (scope, v))
  end

  let singleton_zero a = Map.singleton (module Int) 0 a

  let to_polynomial p ~values ~scoped_values ~eps =
    Map.filter_map p ~f:(fun a ->
        match Expr.calc ~values ~scoped_values (module N) a with
        | c when N.is_finite c -> Some c
        | _ -> None)
    |> Solver.P.of_map ~eps
  ;;
end
