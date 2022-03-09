open Core
open Chapgame

let%test_module "float solver" =
  (module struct
    module SF = Solver.MakeSolver (Float)

    let sample_poly =
      SF.Polynomial.of_list
        [ SF.Monomial.create ~degree:3 ~coefficient:1.
        ; SF.Monomial.create ~degree:1 ~coefficient:(-1.)
        ; SF.Monomial.create ~degree:2 ~coefficient:(-2.)
        ; SF.Monomial.create ~degree:0 ~coefficient:2.
        ]
    ;;

    let%expect_test "to_sexp_test" =
      print_s [%sexp (sample_poly : SF.Polynomial.t)];
      [%expect
        {|
      (((coefficient 1) (degree 3)) ((coefficient -2) (degree 2))
       ((coefficient -1) (degree 1)) ((coefficient 2) (degree 0))) |}]
    ;;

    let%test "of_sexp_test" =
      let sample_poly_from_sexp =
        {|
      (((coefficient 1) (degree 3)) ((coefficient -2) (degree 2))
       ((coefficient -1) (degree 1)) ((coefficient 2) (degree 0))) |}
        |> Sexp.of_string
        |> SF.Polynomial.t_of_sexp
      in
      SF.Polynomial.(equal sample_poly sample_poly_from_sexp)
    ;;

    let%test "of_sexp_test (unordered)" =
      let sample_poly_from_sexp =
        {|
      ( ((coefficient -2) (degree 2))
       ((coefficient -1) (degree 1)) ((coefficient 1) (degree 3)) ((coefficient 2) (degree 0))) |}
        |> Sexp.of_string
        |> SF.Polynomial.t_of_sexp
      in
      SF.Polynomial.(equal sample_poly sample_poly_from_sexp)
    ;;

    let%expect_test "derivative" =
      print_s [%sexp (SF.Polynomial.derivative sample_poly : SF.Polynomial.t)];
      [%expect
        {|
        (((coefficient 3) (degree 2)) ((coefficient -4) (degree 1))
         ((coefficient -1) (degree 0))) |}]
    ;;

    let%expect_test "calc" =
      let print_calc x = printf "x:%f y:%f\n" x @@ SF.Polynomial.calc ~x sample_poly in
      print_calc (-2.);
      print_calc (-1.);
      print_calc 0.;
      print_calc 1.;
      print_calc 2.;
      print_calc 3.;
      print_calc 1.5;
      [%expect {|
        x:-2.000000 y:-12.000000
        x:-1.000000 y:0.000000
        x:0.000000 y:2.000000
        x:1.000000 y:0.000000
        x:2.000000 y:0.000000
        x:3.000000 y:8.000000
        x:1.500000 y:-0.625000 |}]
    ;;

    let%expect_test "1" =
      print_s [%sexp (SF.PolynomialEquation.roots ~eps:1e-7 sample_poly : float list)];
      [%expect {| () |}]
    ;;
  end)
;;
