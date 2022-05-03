open Core
open Chapgame

let%test_module "float solver" =
  (module struct
    module SF = Solver.MakeSolver (Float)

    let eps = 1e-3

    let _print_calcs poly =
      let print_calc x = printf "x:%f y:%f\n" x @@ SF.Polynomial.calc ~x poly in
      List.iter ~f:print_calc
    ;;

    let sample_poly = SF.Polynomial.of_list [ 3, 1.; 1, -1.; 2, -2.; 0, 2. ] ~eps
    let sample_poly_d1 = SF.Polynomial.derivative sample_poly
    let sample_poly_d2 = SF.Polynomial.derivative sample_poly_d1
    let sample_poly_d3 = SF.Polynomial.derivative sample_poly_d2
    let sample_poly_d4 = SF.Polynomial.derivative sample_poly_d3

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
      [%expect
        {|
        x:-0.999918 y:0.000490 lteps:true
         x:0.999969 y:0.000061 lteps:true
         x:1.999756 y:-0.000733 lteps:true |}]
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
          [%expect
            {|
            x:-0.215250 y:0.000000 lteps:true
             x:1.548584 y:0.000000 lteps:true |}]
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
      [%expect
        {|
        (-0.21525043702153024 1.5485837703548635)
        ((NegInfinity (right -0.21525043702153024))
         (Interval (left -0.21525043702153024) (right 1.5485837703548635))
         (PosInfinity (left 1.5485837703548635))) |}];
      print_s [%sexp (SF.Interval.intervals_of_list [] : SF.Interval.t list)];
      [%expect {| (Infinity) |}];
      print_s [%sexp (SF.Interval.intervals_of_list [ -91. ] : SF.Interval.t list)];
      [%expect {| ((NegInfinity (right -91)) (PosInfinity (left -91))) |}]
    ;;

    let%test_module "-4 1 3 5" =
      (module struct
        let pd0 = SF.Polynomial.of_list [ 0, 60.; 1, 43.; 2, -21.; 3, -3.; 4, 1. ] ~eps
        let pd1 = SF.Polynomial.derivative pd0

        let%expect_test "" =
          let rootsd1 = SF.PolynomialEquation.roots pd1 ~eps in
          print_s [%sexp (rootsd1 : float list)];
          [%expect {| (-2.8215947105121373 0.917061554293662 4.1545414878558873) |}];
          let intervals = SF.Interval.intervals_of_list rootsd1 in
          print_s [%sexp (intervals : SF.Interval.t list)];
          [%expect
            {|
            ((NegInfinity (right -2.8215947105121373))
             (Interval (left -2.8215947105121373) (right 0.917061554293662))
             (Interval (left 0.917061554293662) (right 4.1545414878558873))
             (PosInfinity (left 4.1545414878558873))) |}]
        ;;
      end)
    ;;

    let%test_module "fails" =
      (module struct
        let%expect_test "1" =
          let sexp =
            {|((0 25746835.984231804) (1 4577291.1195623642) (2 101561.48085614311) (3 -9013.8329630027256) (4 99.999999999999986))|}
          in
          let poly = SF.Polynomial.t_of_sexp (Sexp.of_string sexp) in
          let eps = 1e-7 in
          let roots = SF.PolynomialEquation.roots ~eps poly in
          print_s [%sexp (roots : float list)];
          [%expect
            {|
            (-9.6463872572101774 -9.0187820495752824 54.087946864588488
             54.715552072225634) |}]
        ;;
      end)
    ;;
  end)
;;
