open Core
open Chapgame
module SC = Scene.Make (Float)
module SF = Solver.MakeSolver (Float)

let%expect_test "" =
  let a = SC.Model.empty ~g:10. in
  let id1, id2 = SC.Figure2.Id.(next (), next ()) in
  let a, _ =
    SC.Engine.recv
      a
      { time = 5.
      ; action = SC.Action.AddBody { id = id1; x0 = 1.; y0 = 1.; r = 1.; mu = 0.01 }
      }
  in
  let a, _ =
    SC.Engine.recv
      a
      { time = 10.
      ; action = SC.Action.AddBody { id = id2; x0 = 7.; y0 = 5.; r = 2.; mu = 1. }
      }
  in
  let a, _ =
    SC.Engine.recv
      a
      { time = 10.; action = SC.Action.GiveVelocity { id = id1; v0 = 2., 2. } }
  in
  let a, _ =
    SC.Engine.recv
      a
      { time = 10.; action = SC.Action.GiveVelocity { id = id2; v0 = -1., -1. } }
  in
  let _elt, els = Map.max_elt_exn a in
  print_s [%sexp (els : SC.Scene.t)];
  [%expect
    {|
    ((figures
      ((0
        ((id 0)
         (values
          ((r (Scalar 2)) (mu (Scalar 1)) (v0 (Vector (-1 -1))) (x0 (Scalar 7))
           (y0 (Scalar 5))))
         (xy
          (((interval
             (Interval
              ((ScalarZero)
               (Div (LengthOfVector (VectorVar v0))
                (LengthOfVector
                 (Mult (Neg (UnitVector (VectorVar v0)))
                  (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                   (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))))))
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
                (ScalarConst 0.5))))))
           ((interval
             (PosInfinity
              (Div (LengthOfVector (VectorVar v0))
               (LengthOfVector
                (Mult (Neg (UnitVector (VectorVar v0)))
                 (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                  (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))))))))
            (x
             ((0
               (Sum (ScalarVar x0)
                (Div (Mult (ScalarConst 3) (Sqr (XOfVector (VectorVar v0))))
                 (Mult (ScalarConst 2)
                  (XOfVector
                   (Mult (Neg (UnitVector (VectorVar v0)))
                    (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                     (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))))))))
            (y
             ((0
               (Sum (ScalarVar y0)
                (Div (Mult (ScalarConst 3) (Sqr (YOfVector (VectorVar v0))))
                 (Mult (ScalarConst 2)
                  (YOfVector
                   (Mult (Neg (UnitVector (VectorVar v0)))
                    (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                     (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))))))))))))))))
       (1
        ((id 1)
         (values
          ((r (Scalar 1)) (mu (Scalar 0.01)) (v0 (Vector (2 2))) (x0 (Scalar 1))
           (y0 (Scalar 1))))
         (xy
          (((interval
             (Interval
              ((ScalarZero)
               (Div (LengthOfVector (VectorVar v0))
                (LengthOfVector
                 (Mult (Neg (UnitVector (VectorVar v0)))
                  (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                   (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))))))
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
                (ScalarConst 0.5))))))
           ((interval
             (PosInfinity
              (Div (LengthOfVector (VectorVar v0))
               (LengthOfVector
                (Mult (Neg (UnitVector (VectorVar v0)))
                 (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                  (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))))))))
            (x
             ((0
               (Sum (ScalarVar x0)
                (Div (Mult (ScalarConst 3) (Sqr (XOfVector (VectorVar v0))))
                 (Mult (ScalarConst 2)
                  (XOfVector
                   (Mult (Neg (UnitVector (VectorVar v0)))
                    (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                     (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))))))))
            (y
             ((0
               (Sum (ScalarVar y0)
                (Div (Mult (ScalarConst 3) (Sqr (YOfVector (VectorVar v0))))
                 (Mult (ScalarConst 2)
                  (YOfVector
                   (Mult (Neg (UnitVector (VectorVar v0)))
                    (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                     (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))))))))))))))))))
     (global_values ((g (Scalar 10))))) |}];
  let t = SC.Scene.t ~eps:1e-5 els in
  print_s [%sexp (t : (int * int * float Sequence.t) Sequence.t)];
  [%expect {| ((0 0 ()) (0 1 (1.723139770105945 3.7962412558203482)) (1 0 ()) (1 1 ())) |}];
  let a, _ = SC.Engine.recv a { time = 11.; action = SC.Action.Empty } in
  let _elt, els = Map.max_elt_exn a in
  print_s [%sexp (els : SC.Scene.t)];
  [%expect
    {|
      ((figures
        ((0
          ((id 0)
           (values
            ((r (Scalar 2)) (mu (Scalar 1)) (v0 (Vector (-1 -1)))
             (x0 (Scalar 7.2121320343559638)) (y0 (Scalar 5.2121320343559638))))
           (xy
            (((interval
               (Interval
                ((ScalarZero)
                 (Div (LengthOfVector (VectorVar v0))
                  (LengthOfVector
                   (Mult (Neg (UnitVector (VectorVar v0)))
                    (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                     (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))))))
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
                  (ScalarConst 0.5))))))
             ((interval
               (PosInfinity
                (Div (LengthOfVector (VectorVar v0))
                 (LengthOfVector
                  (Mult (Neg (UnitVector (VectorVar v0)))
                   (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                    (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))))))))
              (x
               ((0
                 (Sum (ScalarVar x0)
                  (Div (Mult (ScalarConst 3) (Sqr (XOfVector (VectorVar v0))))
                   (Mult (ScalarConst 2)
                    (XOfVector
                     (Mult (Neg (UnitVector (VectorVar v0)))
                      (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                       (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))))))))
              (y
               ((0
                 (Sum (ScalarVar y0)
                  (Div (Mult (ScalarConst 3) (Sqr (YOfVector (VectorVar v0))))
                   (Mult (ScalarConst 2)
                    (YOfVector
                     (Mult (Neg (UnitVector (VectorVar v0)))
                      (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                       (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))))))))))))))))
         (1
          ((id 1)
           (values
            ((r (Scalar 1)) (mu (Scalar 0.01)) (v0 (Vector (2 2)))
             (x0 (Scalar 2.9646446609406727)) (y0 (Scalar 2.9646446609406727))))
           (xy
            (((interval
               (Interval
                ((ScalarZero)
                 (Div (LengthOfVector (VectorVar v0))
                  (LengthOfVector
                   (Mult (Neg (UnitVector (VectorVar v0)))
                    (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                     (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))))))
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
                  (ScalarConst 0.5))))))
             ((interval
               (PosInfinity
                (Div (LengthOfVector (VectorVar v0))
                 (LengthOfVector
                  (Mult (Neg (UnitVector (VectorVar v0)))
                   (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                    (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))))))))
              (x
               ((0
                 (Sum (ScalarVar x0)
                  (Div (Mult (ScalarConst 3) (Sqr (XOfVector (VectorVar v0))))
                   (Mult (ScalarConst 2)
                    (XOfVector
                     (Mult (Neg (UnitVector (VectorVar v0)))
                      (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                       (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))))))))
              (y
               ((0
                 (Sum (ScalarVar y0)
                  (Div (Mult (ScalarConst 3) (Sqr (YOfVector (VectorVar v0))))
                   (Mult (ScalarConst 2)
                    (YOfVector
                     (Mult (Neg (UnitVector (VectorVar v0)))
                      (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                       (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))))))))))))))))))
       (global_values ((g (Scalar 10))))) |}];
  let t = SC.Scene.t ~eps:1e-5 els in
  print_s [%sexp (t : (int * int * float Sequence.t) Sequence.t)];
  [%expect {| ((0 0 ()) (0 1 (0.80587578542576921 2.8042370567877697)) (1 0 ()) (1 1 ())) |}]
;;

(* let%expect_test "to_sexp_test" = let a = SC.Model.empty ~g:10. in let _elt, els =
   Map.min_elt_exn a in let f1 = SC.Scene.figures els in Map.iter f1 ~f:(fun b -> print_s
   (SC.Figure2.sexp_of_t b)); [%expect {| |}]; let t1 = SC.Scene.t1 els in Sequence.iter
   t1 ~f:(fun a -> print_s (SC.Formula.sexp_of_t a); print_endline "\n"); [%expect {| |}]
   ;; *)

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
