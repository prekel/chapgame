open Core
open Chapgame

let%test_module "" =
  (module struct
    module Vars = struct
      type t =
        [ `X
        | `Y
        ]
      [@@deriving sexp, equal]
    end

    module Expr = Expr.Make (Vars) (Nothing) (Float)

    let%expect_test "var" =
      let x, _ = Expr.Syntax.scalar_var `X in
      let y, _ = Expr.Syntax.scalar_var `Y in
      let xy = Expr.Syntax.(x - y) in
      let q1 =
        Expr.calc
          ~values:(function
            | `X -> Scalar 1.
            | `Y -> Scalar 2.)
          ~global_values:(fun _ -> assert false)
          ~scoped_values:never_returns
          (module Float)
          xy
      in
      print_s [%sexp (xy : Expr.t)];
      print_s [%sexp (q1 : float)];
      [%expect {|
        (Sub (ScalarVar X) (ScalarVar Y))
        -1 |}]
    ;;
  end)
;;
