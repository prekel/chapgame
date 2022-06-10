open Core
open Expr

let%test_module "" =
  (module struct
    module Vector = Common.Vector.Make (Float)

    module Var = struct
      type t =
        [ `X
        | `Y
        ]
      [@@deriving sexp, equal, compare]

      include Comparable.Make (struct
        type nonrec t =
          [ `X
          | `Y
          ]
        [@@deriving sexp, equal, compare]
      end)
    end

    module Scope = struct
      include Nothing

      let is_global _ = true
    end

    module Expr = Coeff.Make (Float) (Var) (Scope)
    module Poly = Polynomial.Make (Float) (Var) (Scope) (Expr)
    module Polynomial = Solver.Polynomial.Make (Float)
    module I = Solver.Interval.Make (Float)
    module BS = Solver.Bisection.Make (Float) (I)
    module Equation = Solver.Polynomial_equation.Make (Float) (I) (Polynomial) (BS)

    let%expect_test "((24 * 4) - 24 + 8) / 16 = 5" =
      let open Expr.Syntax in
      let x = scalar_var `X in
      let y = scalar_var `Y in
      let expr = ((x * y) - x + scalar_const 8.) / sqr y in
      let result =
        Expr.calc
          ~values:(function
            | `X -> 24.
            | `Y -> 4.)
          ~scoped_values:never_returns
          (module Float)
          expr
      in
      print_s [%sexp (result : float)];
      [%expect {| 5 |}]
    ;;

    let%expect_test "P_1(t) P_2(t) - P_1(t) + P_2(t)" =
      let open Expr.Syntax in
      let x = scalar_var `X in
      let y = scalar_var `Y in
      let coeff1 = -x + y in
      let coeff2 = x * -y in
      let coeff3 = x - y in
      let poly1 = Poly.of_alist_exn [ 2, coeff1; 1, coeff2 ] in
      let poly2 = Poly.of_alist_exn [ 2, coeff3; 1, coeff2; 0, coeff1 ] in
      let poly = Poly.Syntax.((poly1 * poly2) - poly1 + poly2) in
      let poly_numbers =
        Poly.to_map
          poly
          ~values:(function
            | `X -> 24.
            | `Y -> 4.)
          ~scoped_values:never_returns
        |> Polynomial.of_map ~eps:1e-7
      in
      print_s [%sexp (Polynomial.to_string_hum ~var:"t" poly_numbers : string)];
      [%expect {| -400t^4+9_656t^2+1_920t^1-20t^0 |}];
      let roots = Equation.roots poly_numbers ~eps:1e-7 in
      print_s [%sexp (roots : float list)];
      [%expect
        {|
        (-4.8104056368759291 -0.20912339659875756 0.009921606232339078
         5.0096074272772046) |}]
    ;;
  end)
;;
(* (* ((12 * -12) - 12) / sqr -12 = -1.0833333333333333 *) print_s [%sexp (Expr.calc
   ~values:(function | `X -> 12. | `Y -> -12.) ~scoped_values:never_returns (module Float)
   xy : float)]; [%expect {| -1.0833333333333333 |}] ;;

   let%expect_test "vector x y" = let module Expr = Expr.Make (struct type t = [ `X_x |
   `X_y | `Y_x | `Y_y ] [@@deriving sexp, equal] end) (Nothing) (Float) in let x =
   Expr.Syntax.vector_var `X_x `X_y in let y = Expr.Syntax.vector_var `Y_x `Y_y in let xy
   = Expr.Syntax.(((x * y) - x) / sqr y) in print_s [%sexp (xy : Expr.t)]; [%expect {|
   (Div (Sub (Mult (VectorVar X_x X_y) (VectorVar Y_x Y_y)) (VectorVar X_x X_y)) (Sqr
   (VectorVar Y_x Y_y)))|}]; print_s [%sexp (Expr.calc ~values:(function | `X_x -> -10. |
   `X_y -> 10. | `Y_x -> 2. | `Y_y -> -2.) ~scoped_values:never_returns (module Vector) xy
   : float * float)]; [%expect {| (-2.5 -7.5) |}]; print_s [%sexp (Expr.calc
   ~values:(function | `X_x -> 12. | `X_y -> -12. | `Y_x -> -12. | `Y_y -> 1.)
   ~scoped_values:never_returns (module Vector) xy : float * float)]; [%expect {|
   (-1.0833333333333333 0) |}] ;; end) ;; *)
