open Core
open Chapgame
module SC = Scene.Make (Float)
module SF = Solver.MakeSolver (Float)

let%expect_test "" =
  let a = SC.Model.empty ~g:10. in
  let a, _ =
    SC.Engine.recv
      a
      { time = Time_ns.Span.of_sec 5.
      ; action = SC.Action.AddBody { x0 = 1.; y0 = 1.; r = 1.; mu = 0. }
      }
  in
  let a, _ =
    SC.Engine.recv
      a
      { time = Time_ns.Span.of_sec 10.
      ; action = SC.Action.AddBody { x0 = 7.; y0 = 5.; r = 2.; mu = 0. }
      }
  in
  let a, _ =
    SC.Engine.recv
      a
      { time = Time_ns.Span.of_sec 10.
      ; action = SC.Action.GiveVelocity { id = 0; v0 = 2., 2. }
      }
  in
  let a, _ =
    SC.Engine.recv
      a
      { time = Time_ns.Span.of_sec 10.
      ; action = SC.Action.GiveVelocity { id = 1; v0 = -1., -1. }
      }
  in
  let _elt, els = Map.max_elt_exn a in
  print_s [%sexp (els : SC.Scene.t)];
  [%expect{|
    ((figures
      ((0
        ((id 0)
         (values
          ((r (Scalar 1)) (mu (Scalar 0)) (v0 (Vector (2 2))) (x0 (Scalar 1))
           (y0 (Scalar 1))))
         (x
          ((0 (ScalarVar x0)) (1 (XOfVector (VectorVar v0)))
           (2
            (Mult
             (XOfVector
              (Mult (Neg (UnitVector (VectorVar v0)))
               (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))
             (ScalarConst 0.5)))))
         (y
          ((0 (ScalarVar y0)) (1 (YOfVector (VectorVar v0)))
           (2
            (Mult
             (YOfVector
              (Mult (Neg (UnitVector (VectorVar v0)))
               (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))
             (ScalarConst 0.5)))))))
       (1
        ((id 1)
         (values
          ((r (Scalar 2)) (mu (Scalar 0)) (v0 (Vector (-1 -1))) (x0 (Scalar 7))
           (y0 (Scalar 5))))
         (x
          ((0 (ScalarVar x0)) (1 (XOfVector (VectorVar v0)))
           (2
            (Mult
             (XOfVector
              (Mult (Neg (UnitVector (VectorVar v0)))
               (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))
             (ScalarConst 0.5)))))
         (y
          ((0 (ScalarVar y0)) (1 (YOfVector (VectorVar v0)))
           (2
            (Mult
             (YOfVector
              (Mult (Neg (UnitVector (VectorVar v0)))
               (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))
             (ScalarConst 0.5)))))))))
     (global_values ((g (Scalar 10))))) |}];
  let t = SC.Scene.t els in
  print_s [%sexp (t : SC.Solver.Polynomial.t Sequence.t)];
  [%expect{|
    (
      ((0 -4)) 
      ((0 43) (1 -60) (2 18)) 
      ((0 43) (1 -60) (2 18)) 
      ((0 -16))) |}];
  print_s
    [%sexp
      (Sequence.map t ~f:(SF.PolynomialEquation.roots ~eps:1e-5) : float list Sequence.t)];
  [%expect{|
    (() (1.0430571238199871 2.2902762095133467)
     (1.0430571238199871 2.2902762095133467) ()) |}]
;;

let%expect_test "to_sexp_test" =
  let a = SC.Model.empty ~g:10. in
  let _elt, els = Map.min_elt_exn a in
  let f1 = SC.Scene.figures els in
  Map.iter f1 ~f:(fun b -> print_s (SC.Figure2.sexp_of_t b));
  [%expect {| |}];
  let t1 = SC.Scene.t1 els in
  Sequence.iter t1 ~f:(fun a ->
      print_s (SC.Formula.sexp_of_t a);
      print_endline "\n");
  [%expect {| |}]
;;

let%expect_test "var" =
  let module Var = SC.Expr in
  let module Formula = SC.Formula in
  let x, _x_name = Var.Syntax.scalar_var `x0 in
  let y, _y_name = Var.Syntax.scalar_var `y0 in
  let xy = Var.Syntax.(x + y) in
  let sexp = Var.sexp_of_t xy in
  print_s sexp;
  [%expect {| (Sum (ScalarVar x0) (ScalarVar y0)) |}];
  let xy_from_sexp = Var.t_scalar_of_sexp sexp in
  assert (Poly.(xy = xy_from_sexp))
;;
