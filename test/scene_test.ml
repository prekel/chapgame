open Core
open Chapgame
module SC = Scene.Make (Float)
module SF = Solver.MakeSolver (Float)

let%expect_test "" =
  let eps = 1e-7 in
  let a = SC.Model.empty ~g:10. in
  let id1, id2 = SC.Figure2.Id.(next (), next ()) in
  let a, _ =
    SC.Engine.recv
      ~eps
      a
      { time = 5.
      ; action = SC.Action.AddBody { id = id1; x0 = 1.; y0 = 1.; r = 1.; mu = 0.00001 }
      }
  in
  let a, _ =
    SC.Engine.recv
      ~eps
      a
      { time = 10.
      ; action = SC.Action.AddBody { id = id2; x0 = 7.; y0 = 5.; r = 2.; mu = 0.00001 }
      }
  in
  let a, _ =
    SC.Engine.recv
      ~eps
      a
      { time = 10.; action = SC.Action.GiveVelocity { id = id1; v0 = 2., 2. } }
  in
  let a, _ =
    SC.Engine.recv
      ~eps
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
          ((r (Scalar 2)) (mu (Scalar 1E-05)) (v0 (Vector (-1 -1)))
           (x0 (Scalar 7)) (y0 (Scalar 5))))
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
                (ScalarConst 0.5)))))
            (v_x
             ((0 (XOfVector (VectorVar v0)))
              (1
               (XOfVector
                (Mult (Neg (UnitVector (VectorVar v0)))
                 (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                  (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))))))))
            (v_y
             ((0 (YOfVector (VectorVar v0)))
              (1
               (YOfVector
                (Mult (Neg (UnitVector (VectorVar v0)))
                 (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                  (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))))))
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
                     (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))))))))
            (v_x ()) (v_y ()))))))
       (1
        ((id 1)
         (values
          ((r (Scalar 1)) (mu (Scalar 1E-05)) (v0 (Vector (2 2))) (x0 (Scalar 1))
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
                (ScalarConst 0.5)))))
            (v_x
             ((0 (XOfVector (VectorVar v0)))
              (1
               (XOfVector
                (Mult (Neg (UnitVector (VectorVar v0)))
                 (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                  (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))))))))
            (v_y
             ((0 (YOfVector (VectorVar v0)))
              (1
               (YOfVector
                (Mult (Neg (UnitVector (VectorVar v0)))
                 (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                  (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))))))
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
                     (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))))))))
            (v_x ()) (v_y ()))))))))
     (global_values ((g (Scalar 10))))) |}];
  let t = SC.Scene.t ~eps:1e-5 els in
  print_s [%sexp (t : (int * int * float Sequence.t) Sequence.t)];
  [%expect
    {|
      ((0 0 ())
       (0 1
        (1.0430824126779044 2.2903997684001469 14145.265140131578
         14149.008121436898))
       (1 0 (1.0430824126779044 2.2903997684001469)) (1 1 ())) |}];
  let a, _ = SC.Engine.recv ~eps a { time = 11.; action = SC.Action.Empty } in
  let _elt, els = Map.max_elt_exn a in
  print_s [%sexp (els : SC.Scene.t)];
  [%expect
    {|
        ((figures
          ((0
            ((id 0)
             (values
              ((r (Scalar 2)) (mu (Scalar 1E-05))
               (v0 (Vector (-0.9999292893218813 -0.9999292893218813)))
               (x0 (Scalar 6.0000353553390591)) (y0 (Scalar 4.0000353553390591))))
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
                    (ScalarConst 0.5)))))
                (v_x
                 ((0 (XOfVector (VectorVar v0)))
                  (1
                   (XOfVector
                    (Mult (Neg (UnitVector (VectorVar v0)))
                     (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                      (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))))))))
                (v_y
                 ((0 (YOfVector (VectorVar v0)))
                  (1
                   (YOfVector
                    (Mult (Neg (UnitVector (VectorVar v0)))
                     (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                      (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))))))
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
                         (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))))))))
                (v_x ()) (v_y ()))))))
           (1
            ((id 1)
             (values
              ((r (Scalar 1)) (mu (Scalar 1E-05))
               (v0 (Vector (1.9999292893218814 1.9999292893218814)))
               (x0 (Scalar 2.9999646446609405)) (y0 (Scalar 2.9999646446609405))))
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
                    (ScalarConst 0.5)))))
                (v_x
                 ((0 (XOfVector (VectorVar v0)))
                  (1
                   (XOfVector
                    (Mult (Neg (UnitVector (VectorVar v0)))
                     (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                      (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))))))))
                (v_y
                 ((0 (YOfVector (VectorVar v0)))
                  (1
                   (YOfVector
                    (Mult (Neg (UnitVector (VectorVar v0)))
                     (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                      (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))))))
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
                         (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))))))))
                (v_x ()) (v_y ()))))))))
         (global_values ((g (Scalar 10))))) |}];
  let t = SC.Scene.t ~eps:1e-5 els in
  print_s [%sexp (t : (int * int * float Sequence.t) Sequence.t)];
  [%expect
    {|
      ((0 0 ()) (0 1 (0.043082412677904358 1.2903997684001474 14144.006885188992))
       (1 0 (0.043082412677904358 1.2903997684001474)) (1 1 ())) |}];
  let a, _ = SC.Engine.recv ~eps a { time = 12.; action = SC.Action.Empty } in
  let _elt, els = Map.max_elt_exn a in
  print_s [%sexp (els : SC.Scene.t)];
  [%expect
    {|
          ((figures
            ((0
              ((id 0)
               (values
                ((r (Scalar 2)) (mu (Scalar 1E-05))
                 (v0 (Vector (-0.99992624291155829 -0.99992624291155829)))
                 (x0 (Scalar 5.9569557194192146)) (y0 (Scalar 3.9569557194192151))))
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
                      (ScalarConst 0.5)))))
                  (v_x
                   ((0 (XOfVector (VectorVar v0)))
                    (1
                     (XOfVector
                      (Mult (Neg (UnitVector (VectorVar v0)))
                       (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                        (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))))))))
                  (v_y
                   ((0 (YOfVector (VectorVar v0)))
                    (1
                     (YOfVector
                      (Mult (Neg (UnitVector (VectorVar v0)))
                       (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                        (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))))))
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
                           (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))))))))
                  (v_x ()) (v_y ()))))))
             (1
              ((id 1)
               (values
                ((r (Scalar 1)) (mu (Scalar 1E-05))
                 (v0 (Vector (1.9999262429115583 1.9999262429115583)))
                 (x0 (Scalar 3.0861270285348157)) (y0 (Scalar 3.0861270285348157))))
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
                      (ScalarConst 0.5)))))
                  (v_x
                   ((0 (XOfVector (VectorVar v0)))
                    (1
                     (XOfVector
                      (Mult (Neg (UnitVector (VectorVar v0)))
                       (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                        (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))))))))
                  (v_y
                   ((0 (YOfVector (VectorVar v0)))
                    (1
                     (YOfVector
                      (Mult (Neg (UnitVector (VectorVar v0)))
                       (VectorOfXY (Mult (ScalarVar mu) (Scope -1 (ScalarVar g)))
                        (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))))))
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
                           (Mult (ScalarVar mu) (Scope -1 (ScalarVar g))))))))))))
                  (v_x ()) (v_y ()))))))))
           (global_values ((g (Scalar 10))))) |}];
  let t = SC.Scene.t ~eps:1e-5 els in
  print_s [%sexp (t : (int * int * float Sequence.t) Sequence.t)];
  [%expect
    {|
    ((0 0 ()) (0 1 (1.2473170204461161 14143.791449676664))
     (1 0 (1.2473170204461161)) (1 1 ())) |}]
;;

let%expect_test "to_sexp_test" =
  let eps = 1e-6 in
  let a = SC.Model.empty ~g:10. in
  let id1, id2 = 0, 1 in
  let recv_and_print a b =
    let ret, _ = SC.Engine.recv ~eps a b in
    let r =
      Map.to_sequence ret
      |> Sequence.map ~f:(fun (time, scene) ->
             ( time
             , scene.figures
               |> Map.to_sequence
               |> Sequence.map ~f:(fun (id, figure) -> id, figure.values) ))
    in
    print_s [%sexp (r : (float * (int * SC.values) Sequence.t) Sequence.t)];
    ret
  in
  let a =
    recv_and_print
      a
      { time = 5.
      ; action = SC.Action.AddBody { id = id1; x0 = 1.; y0 = 1.; r = 1.; mu = 0.00001 }
      }
  in
  [%expect
    {|
    ((0 ())
     (5
      ((0
        ((r (Scalar 1)) (mu (Scalar 1E-05)) (v0 (Vector (0 0))) (x0 (Scalar 1))
         (y0 (Scalar 1))))))) |}];
  let a =
    recv_and_print
      a
      { time = 10.
      ; action = SC.Action.AddBody { id = id2; x0 = 7.; y0 = 5.; r = 2.; mu = 0.00001 }
      }
  in
  [%expect
    {|
    ((0 ())
     (5
      ((0
        ((r (Scalar 1)) (mu (Scalar 1E-05)) (v0 (Vector (0 0))) (x0 (Scalar 1))
         (y0 (Scalar 1))))))
     (10
      ((0
        ((r (Scalar 1)) (mu (Scalar 1E-05)) (v0 (Vector (0 0))) (x0 (Scalar 1))
         (y0 (Scalar 1))))
       (1
        ((r (Scalar 2)) (mu (Scalar 1E-05)) (v0 (Vector (0 0))) (x0 (Scalar 7))
         (y0 (Scalar 5))))))) |}];
  let a =
    recv_and_print
      a
      { time = 10.; action = SC.Action.GiveVelocity { id = id1; v0 = 2., 2. } }
  in
  [%expect
    {|
    ((0 ())
     (5
      ((0
        ((r (Scalar 1)) (mu (Scalar 1E-05)) (v0 (Vector (0 0))) (x0 (Scalar 1))
         (y0 (Scalar 1))))))
     (10
      ((0
        ((r (Scalar 1)) (mu (Scalar 1E-05)) (v0 (Vector (2 2))) (x0 (Scalar 1))
         (y0 (Scalar 1))))
       (1
        ((r (Scalar 2)) (mu (Scalar 1E-05)) (v0 (Vector (0 0))) (x0 (Scalar 7))
         (y0 (Scalar 5))))))) |}];
  let a =
    recv_and_print
      a
      { time = 10.; action = SC.Action.GiveVelocity { id = id2; v0 = -1., -1. } }
  in
  [%expect
    {|
    ((0 ())
     (5
      ((0
        ((r (Scalar 1)) (mu (Scalar 1E-05)) (v0 (Vector (0 0))) (x0 (Scalar 1))
         (y0 (Scalar 1))))))
     (10
      ((0
        ((r (Scalar 1)) (mu (Scalar 1E-05)) (v0 (Vector (2 2))) (x0 (Scalar 1))
         (y0 (Scalar 1))))
       (1
        ((r (Scalar 2)) (mu (Scalar 1E-05)) (v0 (Vector (-1 -1))) (x0 (Scalar 7))
         (y0 (Scalar 5))))))) |}];
  let a = recv_and_print a { time = 11.; action = SC.Action.Empty } in
  [%expect
    {|
    ((0 ())
     (5
      ((0
        ((r (Scalar 1)) (mu (Scalar 1E-05)) (v0 (Vector (0 0))) (x0 (Scalar 1))
         (y0 (Scalar 1))))))
     (10
      ((0
        ((r (Scalar 1)) (mu (Scalar 1E-05)) (v0 (Vector (2 2))) (x0 (Scalar 1))
         (y0 (Scalar 1))))
       (1
        ((r (Scalar 2)) (mu (Scalar 1E-05)) (v0 (Vector (-1 -1))) (x0 (Scalar 7))
         (y0 (Scalar 5))))))
     (11
      ((0
        ((r (Scalar 1)) (mu (Scalar 1E-05))
         (v0 (Vector (1.9999292893218814 1.9999292893218814)))
         (x0 (Scalar 2.9999646446609405)) (y0 (Scalar 2.9999646446609405))))
       (1
        ((r (Scalar 2)) (mu (Scalar 1E-05))
         (v0 (Vector (-0.9999292893218813 -0.9999292893218813)))
         (x0 (Scalar 6.0000353553390591)) (y0 (Scalar 4.0000353553390591))))))) |}];
  let a = recv_and_print a { time = 13.; action = SC.Action.Empty } in
  [%expect
    {|
      ((0 ())
       (5
        ((0
          ((r (Scalar 1)) (mu (Scalar 1E-05)) (v0 (Vector (0 0))) (x0 (Scalar 1))
           (y0 (Scalar 1))))))
       (10
        ((0
          ((r (Scalar 1)) (mu (Scalar 1E-05)) (v0 (Vector (2 2))) (x0 (Scalar 1))
           (y0 (Scalar 1))))
         (1
          ((r (Scalar 2)) (mu (Scalar 1E-05)) (v0 (Vector (-1 -1))) (x0 (Scalar 7))
           (y0 (Scalar 5))))))
       (11
        ((0
          ((r (Scalar 1)) (mu (Scalar 1E-05))
           (v0 (Vector (1.9999292893218814 1.9999292893218814)))
           (x0 (Scalar 2.9999646446609405)) (y0 (Scalar 2.9999646446609405))))
         (1
          ((r (Scalar 2)) (mu (Scalar 1E-05))
           (v0 (Vector (-0.9999292893218813 -0.9999292893218813)))
           (x0 (Scalar 6.0000353553390591)) (y0 (Scalar 4.0000353553390591))))))
       (13
        ((0
          ((r (Scalar 1)) (mu (Scalar 1E-05))
           (v0 (Vector (1.9999262429099778 1.9999262429099778)))
           (x0 (Scalar 3.0861270732366508)) (y0 (Scalar 3.0861270732366508))))
         (1
          ((r (Scalar 2)) (mu (Scalar 1E-05))
           (v0 (Vector (-0.99992624290997778 -0.99992624290997778)))
           (x0 (Scalar 5.9569556970691213)) (y0 (Scalar 3.9569556970691218))))))) |}];
  let _a = recv_and_print a { time = 14.; action = SC.Action.Empty } in
  [%expect
    {|
      ((0 ())
       (5
        ((0
          ((r (Scalar 1)) (mu (Scalar 1E-05)) (v0 (Vector (0 0))) (x0 (Scalar 1))
           (y0 (Scalar 1))))))
       (10
        ((0
          ((r (Scalar 1)) (mu (Scalar 1E-05)) (v0 (Vector (2 2))) (x0 (Scalar 1))
           (y0 (Scalar 1))))
         (1
          ((r (Scalar 2)) (mu (Scalar 1E-05)) (v0 (Vector (-1 -1))) (x0 (Scalar 7))
           (y0 (Scalar 5))))))
       (11
        ((0
          ((r (Scalar 1)) (mu (Scalar 1E-05))
           (v0 (Vector (1.9999292893218814 1.9999292893218814)))
           (x0 (Scalar 2.9999646446609405)) (y0 (Scalar 2.9999646446609405))))
         (1
          ((r (Scalar 2)) (mu (Scalar 1E-05))
           (v0 (Vector (-0.9999292893218813 -0.9999292893218813)))
           (x0 (Scalar 6.0000353553390591)) (y0 (Scalar 4.0000353553390591))))))
       (13
        ((0
          ((r (Scalar 1)) (mu (Scalar 1E-05))
           (v0 (Vector (1.9999262429099778 1.9999262429099778)))
           (x0 (Scalar 3.0861270732366508)) (y0 (Scalar 3.0861270732366508))))
         (1
          ((r (Scalar 2)) (mu (Scalar 1E-05))
           (v0 (Vector (-0.99992624290997778 -0.99992624290997778)))
           (x0 (Scalar 5.9569556970691213)) (y0 (Scalar 3.9569556970691218))))))
       (14
        ((0
          ((r (Scalar 1)) (mu (Scalar 1E-05))
           (v0 (Vector (1.9999262429099778 1.9999262429099778)))
           (x0 (Scalar 3.0861270732366508)) (y0 (Scalar 3.0861270732366508))))
         (1
          ((r (Scalar 2)) (mu (Scalar 1E-05))
           (v0 (Vector (-0.99992624290997778 -0.99992624290997778)))
           (x0 (Scalar 5.9569556970691213)) (y0 (Scalar 3.9569556970691218))))))) |}]
;;
