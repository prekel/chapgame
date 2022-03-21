open Core
open Chapgame

let%test_module "" =
  (module struct
    let%expect_test "scalar x y" =
      let module Expr =
        Expr.Make
          (struct
            type t =
              [ `X
              | `Y
              ]
            [@@deriving sexp, equal]
          end)
          (Nothing)
          (Float)
      in
      let x, _ = Expr.Syntax.scalar_var `X in
      let y, _ = Expr.Syntax.scalar_var `Y in
      let xy = Expr.Syntax.(((x * y) - x) / sqr y) in
      print_s [%sexp (xy : Expr.t)];
      [%expect
      {|
        (Div (Sub (Mult (ScalarVar X) (ScalarVar Y)) (ScalarVar X))
         (Sqr (ScalarVar Y)))|}];
      (* ((-11 * 2) - -11) / sqr 2 = -2.75 *)
      print_s
        [%sexp
          (Expr.calc
             ~values:(function
               | `X -> Scalar (-11.)
               | `Y -> Scalar 2.)
             ~scoped_values:never_returns
             (module Float)
             xy
            : float)];
      [%expect {|
      -2.75 |}];
      (* ((12 * -12) - 12) / sqr -12 = -1.0833333333333333 *)
      print_s
        [%sexp
          (Expr.calc
             ~values:(function
               | `X -> Scalar 12.
               | `Y -> Scalar (-12.))
             ~scoped_values:never_returns
             (module Float)
             xy
            : float)];
      [%expect {|
        -1.0833333333333333 |}]
    ;;

    let%expect_test "vector x y" =
      let module Expr =
        Expr.Make
          (struct
            type t =
              [ `X
              | `Y
              ]
            [@@deriving sexp, equal]
          end)
          (Nothing)
          (Float)
      in
      let x, _ = Expr.Syntax.vector_var `X in
      let y, _ = Expr.Syntax.vector_var `Y in
      let xy = Expr.Syntax.(((x * y) - x) / sqr y) in
      print_s [%sexp (xy : Expr.t)];
      [%expect
        {|
        (Div (Sub (Mult (VectorVar X) (VectorVar Y)) (VectorVar X))
         (Sqr (VectorVar Y)))|}];
      print_s
        [%sexp
          (Expr.calc
             ~values:(function
               | `X -> Vector (-10., 10.)
               | `Y -> Vector (2., -2.))
             ~scoped_values:never_returns
             (module Expr.VectorOps)
             xy
            : float * float)];
      [%expect {|
      (-2.5 -7.5) |}];
      print_s
        [%sexp
          (Expr.calc
             ~values:(function
               | `X -> Vector (12., -12.)
               | `Y -> Vector (-12., 1.))
             ~scoped_values:never_returns
             (module Expr.VectorOps)
             xy
            : float * float)];
      [%expect {|
        (-1.0833333333333333 0) |}]
    ;;
  end)
;;
