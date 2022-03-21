open Core
open Chapgame

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
  let module Solver = Solver.MakeSolver (Float) in
  let module Formula =
    Formula.Make
      (struct
        type t =
          [ `X
          | `Y
          ]
        [@@deriving sexp, equal]
      end)
      (Nothing)
      (Float)
      (Expr)
      (Solver)
  in
  let x, _ = Expr.Syntax.scalar_var `X in
  let y, _ = Expr.Syntax.scalar_var `Y in
  let t1 = Expr.Syntax.(x * y) in
  let t2 = Expr.Syntax.(x + y) in
  let f = Formula.of_alist_exn [ 1, t1; 2, t2 ] in
  print_s [%sexp (f : Formula.t)];
  [%expect
    {|
        ((1 (Mult (ScalarVar X) (ScalarVar Y)))
         (2 (Sum (ScalarVar X) (ScalarVar Y))))|}];
  let p =
    Formula.to_polynomial
      ~values:(function
        | `X -> Scalar 1.
        | `Y -> Scalar 2.)
      ~scoped_values:never_returns
      f
  in
  print_s [%sexp (p : Solver.Polynomial.t)];
  [%expect {| ((1 2) (2 3)) |}];
  print_s [%sexp (Solver.PolynomialEquation.roots p ~eps:1e-7 : float list)];
  [%expect {| (-0.66666670640309644 3.9736429887267853E-08) |}]
;;

let%test_module "" =
  (module struct
    module Expr =
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

    module Solver = Solver.MakeSolver (Float)

    module Formula =
      Formula.Make
        (struct
          type t =
            [ `X
            | `Y
            ]
          [@@deriving sexp, equal]
        end)
        (Nothing)
        (Float)
        (Expr)
        (Solver)

    let%expect_test "" =
      let (x, _), (y, _) = Expr.Syntax.(scalar_var `X, scalar_var `Y) in
      (* f = yt^2+xt *)
      let f = Formula.of_alist_exn [ 1, x; 2, y ] in
      print_s [%sexp (f : Formula.t)];
      [%expect {| ((1 (ScalarVar X)) (2 (ScalarVar Y))) |}];
      (* ff = (yt^2+xt)^2 = (yyt4 + yxt^3 + xyt^3 + xxt^2) *)
      let ff = Formula.Syntax.sqr f in
      print_s [%sexp (ff : Formula.t)];
      [%expect
        {|
        ((2 (SumList ((Mult (ScalarVar X) (ScalarVar X)))))
         (3
          (SumList
           ((Mult (ScalarVar X) (ScalarVar Y)) (Mult (ScalarVar Y) (ScalarVar X)))))
         (4 (SumList ((Mult (ScalarVar Y) (ScalarVar Y)))))) |}];
      let p1 =
        Formula.to_polynomial
          ~values:(function
            | `X -> Scalar 2.
            | `Y -> Scalar 3.)
          ~scoped_values:never_returns
          ff
      in
      print_s [%sexp (p1 : Solver.Polynomial.t)];
      [%expect
        {|
        ((2 4) (3 12) (4 9)) |}]
    ;;
  end)
;;
