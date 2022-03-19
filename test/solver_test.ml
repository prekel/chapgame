open Core
open Chapgame

let%test_module "float solver" =
  (module struct
    module SF = Solver.MakeSolver (Float)

    let _print_calcs poly =
      let print_calc x = printf "x:%f y:%f\n" x @@ SF.Polynomial.calc ~x poly in
      List.iter ~f:print_calc
    ;;

    let sample_poly = SF.Polynomial.of_list [ 3, 1.; 1, -1.; 2, -2.; 0, 2. ]
    let sample_poly_d1 = SF.Polynomial.derivative sample_poly
    let sample_poly_d2 = SF.Polynomial.derivative sample_poly_d1
    let sample_poly_d3 = SF.Polynomial.derivative sample_poly_d2
    let sample_poly_d4 = SF.Polynomial.derivative sample_poly_d3
    let eps = 1e-3

    let%expect_test "to_sexp_test" =
      print_s [%sexp (sample_poly : SF.Polynomial.t)];
      [%expect {|
      ((0 2) (1 -1) (2 -2) (3 1)) |}]
    ;;

    let%test "of_sexp_test" =
      let sample_poly_from_sexp =
        {| ((0 2) (1 -1) (2 -2) (3 1)) |} |> Sexp.of_string |> SF.Polynomial.t_of_sexp
      in
      SF.Polynomial.(equal sample_poly sample_poly_from_sexp)
    ;;

    let%test "of_sexp_test (unordered)" =
      let sample_poly_from_sexp =
        {|( (1 -1) (0 2)  (2 -2) (3 1)) |} |> Sexp.of_string |> SF.Polynomial.t_of_sexp
      in
      SF.Polynomial.(equal sample_poly sample_poly_from_sexp)
    ;;

    let%expect_test "derivative" =
      print_s [%sexp (SF.Polynomial.derivative sample_poly : SF.Polynomial.t)];
      [%expect {|
        ((0 -1) (1 -4) (2 3)) |}]
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
      [%expect
        {|
        x:-2.000000 y:-12.000000
        x:-1.000000 y:0.000000
        x:0.000000 y:2.000000
        x:1.000000 y:0.000000
        x:2.000000 y:0.000000
        x:3.000000 y:8.000000
        x:1.500000 y:-0.625000 |}]
    ;;

    let print_check_roots ~eps poly =
      let roots = SF.PolynomialEquation.roots ~eps poly in
      List.iter roots ~f:(fun x ->
          let y = SF.Polynomial.calc ~x poly in
          printf "x:%f y:%f lteps:%b\n " x y Float.(abs y < eps))
    ;;

    let%expect_test "d0 roots" =
      print_check_roots ~eps sample_poly;
      [%expect{|
        x:-0.999837 y:0.000976 lteps:true
         x:0.999939 y:0.000123 lteps:true
         x:1.999674 y:-0.000976 lteps:true |}]
    ;;

    let%test_module "d1" =
      (module struct
        let sample_poly = sample_poly_d1

        let%expect_test "d1 print" =
          print_s [%sexp (sample_poly : SF.Polynomial.t)];
          [%expect {|
      ((0 -1) (1 -4) (2 3)) |}]
        ;;

        let%expect_test "d1 roots" =
          print_check_roots ~eps sample_poly;
          [%expect{|
            x:-0.215169 y:-0.000429 lteps:true
             x:1.548503 y:-0.000429 lteps:true |}]
        ;;
      end)
    ;;

    let%test_module "d2" =
      (module struct
        let%expect_test "d2 root" =
          print_check_roots ~eps sample_poly_d2;
          [%expect {| x:0.666667 y:0.000000 lteps:true |}]
        ;;

        let%expect_test "d2 linear" =
          print_s [%sexp (SF.LinearEquation.root_poly sample_poly_d2 : float option)];
          [%expect {|   (0.66666666666666663) |}]
        ;;

        let%expect_test "d2 print" =
          print_s [%sexp (sample_poly_d2 : SF.Polynomial.t)];
          [%expect {|
      ((0 -4) (1 6)) |}]
        ;;
      end)
    ;;

    let%test_module "d3" =
      (module struct
        let%expect_test "d3" =
          print_check_roots ~eps sample_poly_d3;
          [%expect {| |}]
        ;;

        let%expect_test "d3 linear" =
          print_s [%sexp (SF.LinearEquation.root_poly sample_poly_d3 : float option)];
          [%expect {|   () |}]
        ;;

        let%expect_test "d3 print" =
          print_s [%sexp (sample_poly_d3 : SF.Polynomial.t)];
          [%expect {|
      ((0 6)) |}]
        ;;
      end)
    ;;

    let%test_module "d4" =
      (module struct
        let%expect_test "d4" =
          print_check_roots ~eps sample_poly_d4;
          [%expect {| |}]
        ;;

        let%expect_test "d4 print" =
          print_s [%sexp (sample_poly_d4 : SF.Polynomial.t)];
          [%expect {|
      () |}]
        ;;
      end)
    ;;

    let%expect_test "intervals_of_list test" =
      let l = SF.PolynomialEquation.roots ~eps sample_poly_d1 in
      print_s [%sexp (l : float list)];
      print_s [%sexp (SF.Interval.intervals_of_list l : SF.Interval.t list)];
      [%expect{|
        (-0.21516927083333337 1.5485026041666665)
        ((NegInfinity (right -0.21516927083333337))
         (Interval (left -0.21516927083333337) (right 1.5485026041666665))
         (PosInfinity (left 1.5485026041666665))) |}];
      print_s [%sexp (SF.Interval.intervals_of_list [] : SF.Interval.t list)];
      [%expect{| (Infinity) |}];
      print_s [%sexp (SF.Interval.intervals_of_list [ -91. ] : SF.Interval.t list)];
      [%expect{| ((NegInfinity (right -91)) (PosInfinity (left -91))) |}]
    ;;
  end)
;;
