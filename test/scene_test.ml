open Core
open Chapgame
module SC = Scene.Make (Float)
module SF = Solver.MakeSolver (Float)

let%expect_test "" =
  let a = SC.Model.e () in
  let _elt, els = Map.min_elt_exn a in
  let t = SC.Scene.t els in
  print_s [%sexp (t : SC.Solver.Polynomial.t Sequence.t)];
  [%expect
    {|
    (((0 -64)) ((0 11) (1 48) (2 16) (4 0.25)) ((0 11) (1 48) (2 16) (4 0.25))
     ((0 -4))) |}];
  print_s
    [%sexp
      (Sequence.map t ~f:(SF.PolynomialEquation.roots ~eps:1e-5) : float list Sequence.t)];
  [%expect
    {|
    (() (-2.4837841987609863 -0.25002431869506836)
     (-2.4837841987609863 -0.25002431869506836) ()) |}]
;;

let%expect_test "to_sexp_test" =
  let a = SC.Model.e () in
  let _elt, els = Map.min_elt_exn a in
  let f1 = SC.Scene.figures els in
  Map.iter f1 ~f:(fun b -> print_s (SC.Figure2.sexp_of_t (fun _ -> Sexp.List []) b));
  [%expect {|
    ((id 0)
     (values
      ((a_vec (Vector (1 -0.5))) (r (Scalar 4)) (v0_vec (Vector (-2 0.5)))
       (x0 (Scalar -3)) (y0 (Scalar 3))))
     (x
      ((0 (ScalarVar x0)) (1 (XOfVector (VectorVar v0_vec)))
       (2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))
     (y
      ((0 (ScalarVar y0)) (1 (YOfVector (VectorVar v0_vec)))
       (2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5))))))
    ((id 1)
     (values
      ((a_vec (Vector (1 0.5))) (r (Scalar 1)) (v0_vec (Vector (2 0.5)))
       (x0 (Scalar 3)) (y0 (Scalar 3))))
     (x
      ((0 (ScalarVar x0)) (1 (XOfVector (VectorVar v0_vec)))
       (2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))
     (y
      ((0 (ScalarVar y0)) (1 (YOfVector (VectorVar v0_vec)))
       (2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))) |}];
  let t1 = SC.Scene.t1 els in
  Sequence.iter t1 ~f:(fun a ->
      Map.iter a ~f:(fun b ->
          print_s
            (SC.Figure2.Formula.Var.sexp_of_t
               (function
                 | `Body1 -> Sexp.Atom "Body1"
                 | `Body2 -> Sexp.Atom "Body2")
               b));
      print_endline "\n");
  [%expect
    {|
      (Sub
       (Sum
        (SumList
         ((Mult (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0)))
           (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0))))))
        (SumList
         ((Mult (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))
           (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))))))
       (SumList
        ((Mult (Sum (Scope Body1 (ScalarVar r)) (Scope Body2 (ScalarVar r)))
          (Sum (Scope Body1 (ScalarVar r)) (Scope Body2 (ScalarVar r)))))))
      (Sum
       (SumList
        ((Mult (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0)))
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec)))))
         (Mult
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0))))))
       (SumList
        ((Mult (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec)))))
         (Mult
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))))))
      (Sum
       (SumList
        ((Mult (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0)))
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))
         (Mult
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec)))))
         (Mult
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0))))))
       (SumList
        ((Mult (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))
         (Mult
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec)))))
         (Mult
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))))))
      (Sum
       (SumList
        ((Mult
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))
         (Mult
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec)))))))
       (SumList
        ((Mult
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))
         (Mult
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec))))))))
      (Sum
       (SumList
        ((Mult
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))))
       (SumList
        ((Mult
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5))))))))


      (Sub
       (Sum
        (SumList
         ((Mult (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0)))
           (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0))))))
        (SumList
         ((Mult (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))
           (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))))))
       (SumList
        ((Mult (Sum (Scope Body1 (ScalarVar r)) (Scope Body2 (ScalarVar r)))
          (Sum (Scope Body1 (ScalarVar r)) (Scope Body2 (ScalarVar r)))))))
      (Sum
       (SumList
        ((Mult (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0)))
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec)))))
         (Mult
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0))))))
       (SumList
        ((Mult (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec)))))
         (Mult
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))))))
      (Sum
       (SumList
        ((Mult (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0)))
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))
         (Mult
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec)))))
         (Mult
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0))))))
       (SumList
        ((Mult (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))
         (Mult
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec)))))
         (Mult
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))))))
      (Sum
       (SumList
        ((Mult
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))
         (Mult
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec)))))))
       (SumList
        ((Mult
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))
         (Mult
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec))))))))
      (Sum
       (SumList
        ((Mult
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))))
       (SumList
        ((Mult
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5))))))))


      (Sub
       (Sum
        (SumList
         ((Mult (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0)))
           (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0))))))
        (SumList
         ((Mult (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))
           (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))))))
       (SumList
        ((Mult (Sum (Scope Body1 (ScalarVar r)) (Scope Body2 (ScalarVar r)))
          (Sum (Scope Body1 (ScalarVar r)) (Scope Body2 (ScalarVar r)))))))
      (Sum
       (SumList
        ((Mult (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0)))
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec)))))
         (Mult
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0))))))
       (SumList
        ((Mult (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec)))))
         (Mult
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))))))
      (Sum
       (SumList
        ((Mult (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0)))
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))
         (Mult
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec)))))
         (Mult
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0))))))
       (SumList
        ((Mult (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))
         (Mult
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec)))))
         (Mult
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))))))
      (Sum
       (SumList
        ((Mult
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))
         (Mult
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec)))))))
       (SumList
        ((Mult
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))
         (Mult
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec))))))))
      (Sum
       (SumList
        ((Mult
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))))
       (SumList
        ((Mult
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5))))))))


      (Sub
       (Sum
        (SumList
         ((Mult (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0)))
           (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0))))))
        (SumList
         ((Mult (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))
           (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))))))
       (SumList
        ((Mult (Sum (Scope Body1 (ScalarVar r)) (Scope Body2 (ScalarVar r)))
          (Sum (Scope Body1 (ScalarVar r)) (Scope Body2 (ScalarVar r)))))))
      (Sum
       (SumList
        ((Mult (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0)))
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec)))))
         (Mult
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0))))))
       (SumList
        ((Mult (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec)))))
         (Mult
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))))))
      (Sum
       (SumList
        ((Mult (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0)))
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))
         (Mult
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec)))))
         (Mult
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (ScalarVar x0)) (Scope Body1 (ScalarVar x0))))))
       (SumList
        ((Mult (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))
         (Mult
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec)))))
         (Mult
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (ScalarVar y0)) (Scope Body1 (ScalarVar y0)))))))
      (Sum
       (SumList
        ((Mult
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))
         (Mult
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (XOfVector (VectorVar v0_vec)))
           (Scope Body1 (XOfVector (VectorVar v0_vec)))))))
       (SumList
        ((Mult
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec))))
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))
         (Mult
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (YOfVector (VectorVar v0_vec)))
           (Scope Body1 (YOfVector (VectorVar v0_vec))))))))
      (Sum
       (SumList
        ((Mult
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (XOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))))
       (SumList
        ((Mult
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5))))
          (Sub (Scope Body2 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))
           (Scope Body1 (Mult (YOfVector (VectorVar a_vec)) (ScalarConst 0.5)))))))) |}]
;;