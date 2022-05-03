open Core
open Chapgame

let%test_module "" =
  (module struct
    module Vector = Vector.Make (Float)

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
      let x = Expr.Syntax.scalar_var `X in
      let y = Expr.Syntax.scalar_var `Y in
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
               | `X -> -11.
               | `Y -> 2.)
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
               | `X -> 12.
               | `Y -> -12.)
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
              [ `X_x
              | `X_y
              | `Y_x
              | `Y_y
              ]
            [@@deriving sexp, equal]
          end)
          (Nothing)
          (Float)
      in
      let x = Expr.Syntax.vector_var `X_x `X_y in
      let y = Expr.Syntax.vector_var `Y_x `Y_y in
      let xy = Expr.Syntax.(((x * y) - x) / sqr y) in
      print_s [%sexp (xy : Expr.t)];
      [%expect
        {|
        (Div (Sub (Mult (VectorVar X_x X_y) (VectorVar Y_x Y_y)) (VectorVar X_x X_y))
         (Sqr (VectorVar Y_x Y_y)))|}];
      print_s
        [%sexp
          (Expr.calc
             ~values:(function
               | `X_x -> -10.
               | `X_y -> 10.
               | `Y_x -> 2.
               | `Y_y -> -2.)
             ~scoped_values:never_returns
             (module Vector)
             xy
            : float * float)];
      [%expect {|
      (-2.5 -7.5) |}];
      print_s
        [%sexp
          (Expr.calc
             ~values:(function
               | `X_x -> 12.
               | `X_y -> -12.
               | `Y_x -> -12.
               | `Y_y -> 1.)
             ~scoped_values:never_returns
             (module Vector)
             xy
            : float * float)];
      [%expect {|
        (-1.0833333333333333 0) |}]
    ;;
  end)
;;
