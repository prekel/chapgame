open Core
include Polynomial_intf

module Make
    (N : Common.Module_types.NUMBER)
    (Var : Module_types.VAR)
    (Scope : Module_types.SCOPE)
    (Coef : Coef.S with module Var = Var and module Scope = Scope and module N = N) =
struct
  module Var = Var
  module Scope = Scope
  module N = N
  module Coef = Coef

  include
    Common.Utils.MakeAdvancedMap
      (Int)
      (struct
        type t = Coef.scalar Coef.t

        let equal = Coef.equal_t_scalar
        let t_of_sexp = Coef.t_scalar_of_sexp
        let sexp_of_t = Coef.sexp_of_t
      end)

  module Syntax = struct
    let ( + ) = Map.merge_skewed ~combine:(fun ~key:_ a b -> Coef.Sum (a, b))
    let ( ~- ) = Map.map ~f:(fun b -> Coef.Neg b)
    let ( - ) a b = a + -b

    let ( * ) a b =
      Sequence.cartesian_product (Map.to_sequence a) (Map.to_sequence b)
      |> Sequence.map ~f:(fun ((d1, c1), (d2, c2)) -> Int.(d1 + d2), Coef.Mult (c1, c2))
      |> Map.of_sequence_multi (module Int)
      |> Map.map ~f:(fun a -> Coef.SumList a)
    ;;

    let sqr a = a * a
    let scope m ~scope = Map.map m ~f:(fun v -> Coef.Scope (scope, v))
  end

  let singleton_zero a = Map.singleton (module Int) 0 a

  let to_map p ~values ~scoped_values =
    Map.filter_map p ~f:(fun a ->
        match Coef.calc ~values ~scoped_values (module N) a with
        | c when N.is_finite c -> Some c
        | _ -> None)
  ;;
end
