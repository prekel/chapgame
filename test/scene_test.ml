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
      ; action = SC.Action.AddBody { x0 = 1.; y0 = 1.; r = 1.; mu = 1. }
      }
  in
  let a, _ =
    SC.Engine.recv
      a
      { time = Time_ns.Span.of_sec 10.
      ; action = SC.Action.AddBody { x0 = 7.; y0 = 5.; r = 2.; mu = 1. }
      }
  in
  let _elt, els = Map.max_elt_exn a in
  print_s [%sexp (els : SC.Scene.t)];
  [%expect {|
    ((figures
      ((0
        ((id 0)
         (values
          ((r (Scalar 1)) (mu (Scalar 1)) (v0 (Vector (0 0))) (x0 (Scalar 1))
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
          ((r (Scalar 2)) (mu (Scalar 1)) (v0 (Vector (0 0))) (x0 (Scalar 7))
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
    (((0 -4) (2 NAN) (3 NAN) (4 NAN)) ((0 43) (2 NAN) (3 NAN) (4 NAN))
     ((0 43) (2 NAN) (3 NAN) (4 NAN)) ((0 -16) (2 NAN) (3 NAN) (4 NAN))) |}];
  print_s
    [%sexp
      (Sequence.map t ~f:(SF.PolynomialEquation.roots ~eps:1e-5) : float list Sequence.t)];
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Float.sign_exn of NAN" -NAN)
  Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 14-30
  Called from Base__Error.raise_s in file "src/error.ml", line 10, characters 19-40
  Called from Chapgame__Solver.MakeSolver.BinarySearch.search.increase_rec in file "common/solver.ml", line 237, characters 42-52
  Called from Base__List.rev_filter_map.loop in file "src/list.ml", line 944, characters 13-17
  Called from Base__List.filter_map in file "src/list.ml" (inlined), line 951, characters 26-47
  Called from Chapgame__Solver.MakeSolver.PolynomialEquation.roots in file "common/solver.ml", line 277, characters 8-170
  Called from Chapgame__Solver.MakeSolver.PolynomialEquation.roots in file "common/solver.ml", line 277, characters 8-67
  Called from Chapgame__Solver.MakeSolver.PolynomialEquation.roots in file "common/solver.ml", line 277, characters 8-67
  Called from Base__Sequence.map.(fun) in file "src/sequence.ml", line 194, characters 33-36
  Called from Base__Sequence.to_list.to_list in file "src/sequence.ml", line 141, characters 12-18
  Called from Base__Sequence.sexp_of_t in file "src/sequence.ml" (inlined), line 149, characters 51-62
  Called from Chapgame_test__Scene_test.(fun) in file "test/scene_test.ml", line 80, characters 67-88
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]
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
