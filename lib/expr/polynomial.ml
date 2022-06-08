open Core
include Polynomial_intf

module Make
    (N : Common.Module_types.NUMBER)
    (Var : Module_types.VAR)
    (Scope : Module_types.SCOPE)
    (Coeff : Coeff.S with module Var = Var and module Scope = Scope and module N = N) =
struct
  module Var = Var
  module Scope = Scope
  module N = N
  module Coeff = Coeff

  include
    Common.Utils.MakeAdvancedMap
      (Int)
      (struct
        type t = Coeff.scalar Coeff.t

        let equal = Coeff.equal_t_scalar
        let t_of_sexp = Coeff.t_scalar_of_sexp
        let sexp_of_t = Coeff.sexp_of_t
      end)

  module Syntax = struct
    let ( + ) = Map.merge_skewed ~combine:(fun ~key:_ a b -> Coeff.Sum (a, b))
    let ( ~- ) = Map.map ~f:(fun b -> Coeff.Neg b)
    let ( - ) a b = a + -b

    let ( * ) a b =
      Sequence.cartesian_product (Map.to_sequence a) (Map.to_sequence b)
      |> Sequence.map ~f:(fun ((d1, c1), (d2, c2)) -> Int.(d1 + d2), Coeff.Mult (c1, c2))
      |> Map.of_sequence_multi (module Int)
      |> Map.map ~f:(fun a -> Coeff.SumList a)
    ;;

    let sqr a = a * a
    let scope m ~scope = Map.map m ~f:(fun v -> Coeff.Scope (scope, v))
  end

  let singleton_zero a = Map.singleton (module Int) 0 a

  let to_map p ~values ~scoped_values =
    Map.filter_map p ~f:(fun a ->
        match Coeff.calc ~values ~scoped_values (module N) a with
        | c when N.is_finite c -> Some c
        | _ -> None)
  ;;
end
