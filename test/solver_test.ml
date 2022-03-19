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
        x:-1.000326 y:-0.001954 lteps:false
         x:0.999692 y:0.000615 lteps:true
         x:1.999674 y:-0.000976 lteps:true |}]
    ;;

    let%test_module "d1" =
      (module struct
        let sample_poly = sample_poly_d1

        let%expect_test "d1 print" =
          print_s [%sexp (sample_poly : SF.Polynomial.t)];
          [%expect
            {|
      (((coefficient 3) (degree 2)) ((coefficient -4) (degree 1))
       ((coefficient -1) (degree 0))) |}]
        ;;

        let%expect_test "d1 roots" =
          print_check_roots ~eps sample_poly;
          [%expect
            {|
            x:-0.215658 y:0.002155 lteps:false
             x:1.548991 y:0.002155 lteps:false |}]
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
          [%expect
            {|
      (((coefficient 6) (degree 1)) ((coefficient -4) (degree 0))) |}]
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
      (((coefficient 6) (degree 0))) |}]
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
        (-0.21565755208333337 1.5489908854166665)
        ((Neg_Infinity -0.21565755208333337)
         (Interval -0.21565755208333337 1.5489908854166665)
         (Pos_Infinity 1.5489908854166665)) |}];
      print_s [%sexp (SF.Interval.intervals_of_list [] : SF.Interval.t list)];
      [%expect {| (Infinity) |}];
      print_s [%sexp (SF.Interval.intervals_of_list [ -91. ] : SF.Interval.t list)];
      [%expect {| ((Neg_Infinity -91) (Pos_Infinity -91)) |}]
    ;;
  end)
;;
