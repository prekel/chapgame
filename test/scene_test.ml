(* open Core
open Chapgame
module SC = Scene.Make (Float)
module SF = Solver.MakeSolver (Float)

let%expect_test "" =
  let eps = 1e-7 in
  let a = SC.Model.empty ~g:10. in
  let id1, id2 = SC.Figure2.Id.(next (), next ()) in
  let a =
    SC.Engine.recv
      ~eps
      a
      ~action:{ time = 5.
      ; action =
          SC.Action.AddBody { id = id1; x0 = 1.; y0 = 1.; r = 1.; mu = 0.00001; m = 1. }
      }
  in
  let a =
    SC.Engine.recv
      ~eps
      a
      ~action:{ time = 10.
      ; action =
          SC.Action.AddBody { id = id2; x0 = 7.; y0 = 5.; r = 2.; mu = 0.00001; m = 1. }
      }
  in
  let a =
    SC.Engine.recv
      ~eps
      a
      ~action:{ time = 10.; action = SC.Action.GiveVelocity { id = id1; v0 = 2., 2. } }
  in
  let a =
    SC.Engine.recv
      ~eps
      a
      ~action:{ time = 10.; action = SC.Action.GiveVelocity { id = id2; v0 = -1., -1. } }
  in
  (* let _elt, els = Map.max_elt_exn a in print_s [%sexp (els : SC.Scene.t)]; *)
  [%expect {| |}];
  (* let t = SC.CollisionDetection.collisions ~eps:1e-5 els in *)
  (* print_s [%sexp (t : (int * int * float Sequence.t) Sequence.t)]; *)
  (* [%expect {| ((0 1 (1.0430824126779044 2.2903997684001469 14145.265140131578
     14149.008121436898)) (1 0 (1.0430824126779044 2.2903997684001469))) |}]; *)
  let a = SC.Engine.recv ~eps a ~action:{ time = 11.; action = SC.Action.Empty } in
  (* let _elt, els = Map.max_elt_exn a in print_s [%sexp (els : SC.Scene.t)]; *)
  [%expect {| |}];
  (* let t = SC.Scene.t ~eps:1e-5 els in print_s [%sexp (t : (int * int * float
     Sequence.t) Sequence.t)]; [%expect {| ((0 1 (0.043082412677904358 1.2903997684001474
     14144.006885188992)) (1 0 (0.043082412677904358 1.2903997684001474))) |}]; *)
  let _a = SC.Engine.recv ~eps a ~action:{ time = 12.; action = SC.Action.Empty } in
  (* let _elt, els = Map.max_elt_exn a in print_s [%sexp (els : SC.Scene.t)]; *)
  [%expect {| |}]
;;

(* ; *)
(* let t = SC.Scene.t ~eps:1e-5 els in print_s [%sexp (t : (int * int * float Sequence.t)
   Sequence.t)]; [%expect {| ((0 1 ()) (1 0 ())) |}] *)

type scene1 =
  { time : float
  ; bodies : (int * SC.Values.t) Sequence.t
  ; global_values : SC.Values.t
  ; cause : SC.Scene.Cause.t list
  }
[@@deriving sexp_of]

let%expect_test "test 2" =
  let eps = 1e-9 in
  let a = SC.Model.empty ~g:10. in
  let id1, id2 = 0, 1 in
  let recv_and_print a b =
    let ret = SC.Engine.recv ~eps a ~action:b in
    let r =
      ret
      |> List.rev
      |> Sequence.of_list
      |> Sequence.map ~f:(fun ({ time; _ } as scene) ->
             ( time
             , { time = scene.time
               ; bodies =
                   scene.figures
                   |> SC.Scene.Figures.to_map
                   |> Map.to_sequence
                   |> Sequence.map ~f:(fun (id, figure) -> id, figure.values)
               ; global_values = scene.global_values
               ; cause = scene.cause
               } ))
    in
    print_s [%sexp (r : (float * scene1) Sequence.t)];
    ret
  in
  let a =
    recv_and_print
      a
      { time = 5.
      ; action =
          SC.Action.AddBody { id = id1; x0 = 1.; y0 = 1.; r = 1.; mu = 0.000005; m = 1. }
      }
  in
  [%expect
    {|
    ((0 ((time 0) (bodies ()) (global_values ((g (Scalar 10)))) (cause (Init))))
     (5
      ((time 5)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06)) (v0 (Vector (0 0)))
           (x0 (Scalar 1)) (y0 (Scalar 1))))))
       (global_values ((g (Scalar 10)))) (cause ((BodyAdded (id 0)) Empty))))) |}];
  let a =
    recv_and_print
      a
      { time = 10.
      ; action =
          SC.Action.AddBody
            { id = id2; x0 = 7.; y0 = 5.; r = 2.; mu = 0.0000000005; m = 2. }
      }
  in
  [%expect
    {|
    ((0 ((time 0) (bodies ()) (global_values ((g (Scalar 10)))) (cause (Init))))
     (5
      ((time 5)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06)) (v0 (Vector (0 0)))
           (x0 (Scalar 1)) (y0 (Scalar 1))))))
       (global_values ((g (Scalar 10)))) (cause ((BodyAdded (id 0)) Empty))))
     (10
      ((time 10)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06)) (v0 (Vector (0 0)))
           (x0 (Scalar 1)) (y0 (Scalar 1))))
         (1
          ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10)) (v0 (Vector (0 0)))
           (x0 (Scalar 7)) (y0 (Scalar 5))))))
       (global_values ((g (Scalar 10)))) (cause ((BodyAdded (id 1)) Empty))))) |}];
  let a =
    recv_and_print
      a
      { time = 10.; action = SC.Action.GiveVelocity { id = id1; v0 = 2., 2. } }
  in
  [%expect
    {|
    ((0 ((time 0) (bodies ()) (global_values ((g (Scalar 10)))) (cause (Init))))
     (5
      ((time 5)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06)) (v0 (Vector (0 0)))
           (x0 (Scalar 1)) (y0 (Scalar 1))))))
       (global_values ((g (Scalar 10)))) (cause ((BodyAdded (id 0)) Empty))))
     (10
      ((time 10)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06)) (v0 (Vector (2 2)))
           (x0 (Scalar 1)) (y0 (Scalar 1))))
         (1
          ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10)) (v0 (Vector (0 0)))
           (x0 (Scalar 7)) (y0 (Scalar 5))))))
       (global_values ((g (Scalar 10))))
       (cause ((VelocityGiven (id 0) (v (2 2))) (BodyAdded (id 1)) Empty))))) |}];
  let a =
    recv_and_print
      a
      { time = 10.; action = SC.Action.GiveVelocity { id = id2; v0 = -1., -1. } }
  in
  [%expect
    {|
    ((0 ((time 0) (bodies ()) (global_values ((g (Scalar 10)))) (cause (Init))))
     (5
      ((time 5)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06)) (v0 (Vector (0 0)))
           (x0 (Scalar 1)) (y0 (Scalar 1))))))
       (global_values ((g (Scalar 10)))) (cause ((BodyAdded (id 0)) Empty))))
     (10
      ((time 10)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06)) (v0 (Vector (2 2)))
           (x0 (Scalar 1)) (y0 (Scalar 1))))
         (1
          ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
           (v0 (Vector (-1 -1))) (x0 (Scalar 7)) (y0 (Scalar 5))))))
       (global_values ((g (Scalar 10))))
       (cause
        ((VelocityGiven (id 1) (v (-1 -1))) (VelocityGiven (id 0) (v (2 2)))
         (BodyAdded (id 1)) Empty))))) |}];
  let a = recv_and_print a { time = 11.; action = SC.Action.Empty } in
  (* let t = SC.Scene.t ~eps (snd @@ Map.max_elt_exn a) in print_s [%sexp (t : (int * int
     * float Sequence.t) Sequence.t)]; *)
  [%expect
    {|
    ((0 ((time 0) (bodies ()) (global_values ((g (Scalar 10)))) (cause (Init))))
     (5
      ((time 5)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06)) (v0 (Vector (0 0)))
           (x0 (Scalar 1)) (y0 (Scalar 1))))))
       (global_values ((g (Scalar 10)))) (cause ((BodyAdded (id 0)) Empty))))
     (10
      ((time 10)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06)) (v0 (Vector (2 2)))
           (x0 (Scalar 1)) (y0 (Scalar 1))))
         (1
          ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
           (v0 (Vector (-1 -1))) (x0 (Scalar 7)) (y0 (Scalar 5))))))
       (global_values ((g (Scalar 10))))
       (cause
        ((VelocityGiven (id 1) (v (-1 -1))) (VelocityGiven (id 0) (v (2 2)))
         (BodyAdded (id 1)) Empty))))
     (11
      ((time 11)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06))
           (v0 (Vector (1.9999646446609407 1.9999646446609407)))
           (x0 (Scalar 2.9999823223304705)) (y0 (Scalar 2.9999823223304705))))
         (1
          ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
           (v0 (Vector (-0.99999999646446613 -0.99999999646446613)))
           (x0 (Scalar 6.0000000017677673)) (y0 (Scalar 4.0000000017677673))))))
       (global_values ((g (Scalar 10)))) (cause (Empty Empty))))) |}];
  let a = recv_and_print a { time = 13.; action = SC.Action.Empty } in
  (* let t = SC.Scene.t ~eps (snd @@ Map.max_elt_exn a) in print_s [%sexp (t : (int * int
     * float Sequence.t) Sequence.t)]; *)
  [%expect
    {|
      ((0 ((time 0) (bodies ()) (global_values ((g (Scalar 10)))) (cause (Init))))
       (5
        ((time 5)
         (bodies
          ((0
            ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06)) (v0 (Vector (0 0)))
             (x0 (Scalar 1)) (y0 (Scalar 1))))))
         (global_values ((g (Scalar 10)))) (cause ((BodyAdded (id 0)) Empty))))
       (10
        ((time 10)
         (bodies
          ((0
            ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06)) (v0 (Vector (2 2)))
             (x0 (Scalar 1)) (y0 (Scalar 1))))
           (1
            ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
             (v0 (Vector (-1 -1))) (x0 (Scalar 7)) (y0 (Scalar 5))))))
         (global_values ((g (Scalar 10))))
         (cause
          ((VelocityGiven (id 1) (v (-1 -1))) (VelocityGiven (id 0) (v (2 2)))
           (BodyAdded (id 1)) Empty))))
       (11
        ((time 11)
         (bodies
          ((0
            ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06))
             (v0 (Vector (1.9999646446609407 1.9999646446609407)))
             (x0 (Scalar 2.9999823223304705)) (y0 (Scalar 2.9999823223304705))))
           (1
            ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
             (v0 (Vector (-0.99999999646446613 -0.99999999646446613)))
             (x0 (Scalar 6.0000000017677673)) (y0 (Scalar 4.0000000017677673))))))
         (global_values ((g (Scalar 10)))) (cause (Empty Empty))))
       (11.043063513818197
        ((time 11.043063513818197)
         (bodies
          ((0
            ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06))
             (v0 (Vector (-2.77404813585298 -1.1110995224660645)))
             (x0 (Scalar 3.0861077946590911)) (y0 (Scalar 3.0861077946590911))))
           (1
            ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
             (v0 (Vector (1.3870056326821811 0.55554665451495555)))
             (x0 (Scalar 5.956936488105101)) (y0 (Scalar 3.956936488105101))))))
         (global_values ((g (Scalar 10)))) (cause ((Collision (id1 0) (id2 1))))))
       (13
        ((time 13)
         (bodies
          ((0
            ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06))
             (v0 (Vector (-2.7739573040803935 -1.1110631412880161)))
             (x0 (Scalar -2.3424393408112807)) (y0 (Scalar 0.91179219719343807))))
           (1
            ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
             (v0 (Vector (1.3870056235990136 0.55554665087681365)))
             (x0 (Scalar 8.6712184083529458)) (y0 (Scalar 5.0441060025418478))))))
         (global_values ((g (Scalar 10)))) (cause (Empty Empty))))) |}];
  let a =
    recv_and_print
      a
      { time = 14.; action = SC.Action.GiveVelocity { id = id1; v0 = 3., 1. } }
  in
  (* let t = SC.Scene.t ~eps (snd @@ Map.max_elt_exn a) in print_s [%sexp (t : (int * int
     * float Sequence.t) Sequence.t)]; *)
  [%expect
    {|
      ((0 ((time 0) (bodies ()) (global_values ((g (Scalar 10)))) (cause (Init))))
       (5
        ((time 5)
         (bodies
          ((0
            ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06)) (v0 (Vector (0 0)))
             (x0 (Scalar 1)) (y0 (Scalar 1))))))
         (global_values ((g (Scalar 10)))) (cause ((BodyAdded (id 0)) Empty))))
       (10
        ((time 10)
         (bodies
          ((0
            ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06)) (v0 (Vector (2 2)))
             (x0 (Scalar 1)) (y0 (Scalar 1))))
           (1
            ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
             (v0 (Vector (-1 -1))) (x0 (Scalar 7)) (y0 (Scalar 5))))))
         (global_values ((g (Scalar 10))))
         (cause
          ((VelocityGiven (id 1) (v (-1 -1))) (VelocityGiven (id 0) (v (2 2)))
           (BodyAdded (id 1)) Empty))))
       (11
        ((time 11)
         (bodies
          ((0
            ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06))
             (v0 (Vector (1.9999646446609407 1.9999646446609407)))
             (x0 (Scalar 2.9999823223304705)) (y0 (Scalar 2.9999823223304705))))
           (1
            ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
             (v0 (Vector (-0.99999999646446613 -0.99999999646446613)))
             (x0 (Scalar 6.0000000017677673)) (y0 (Scalar 4.0000000017677673))))))
         (global_values ((g (Scalar 10)))) (cause (Empty Empty))))
       (11.043063513818197
        ((time 11.043063513818197)
         (bodies
          ((0
            ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06))
             (v0 (Vector (-2.77404813585298 -1.1110995224660645)))
             (x0 (Scalar 3.0861077946590911)) (y0 (Scalar 3.0861077946590911))))
           (1
            ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
             (v0 (Vector (1.3870056326821811 0.55554665451495555)))
             (x0 (Scalar 5.956936488105101)) (y0 (Scalar 3.956936488105101))))))
         (global_values ((g (Scalar 10)))) (cause ((Collision (id1 0) (id2 1))))))
       (13
        ((time 13)
         (bodies
          ((0
            ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06))
             (v0 (Vector (-2.7739573040803935 -1.1110631412880161)))
             (x0 (Scalar -2.3424393408112807)) (y0 (Scalar 0.91179219719343807))))
           (1
            ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
             (v0 (Vector (1.3870056235990136 0.55554665087681365)))
             (x0 (Scalar 8.6712184083529458)) (y0 (Scalar 5.0441060025418478))))))
         (global_values ((g (Scalar 10)))) (cause (Empty Empty))))
       (14
        ((time 14)
         (bodies
          ((0
            ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06)) (v0 (Vector (3 1)))
             (x0 (Scalar -5.1163734372471676)) (y0 (Scalar -0.1992616486528748))))
           (1
            ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
             (v0 (Vector (1.3870056189574895 0.555546649017713)))
             (x0 (Scalar 10.058224029631196)) (y0 (Scalar 5.5996526524891106))))))
         (global_values ((g (Scalar 10))))
         (cause ((VelocityGiven (id 0) (v (3 1))) Empty))))) |}];
  let a = recv_and_print a { time = 17.2; action = SC.Action.Empty } in
  (* let t = SC.Scene.t ~eps (snd @@ Map.max_elt_exn a) in print_s [%sexp (t : (int * int
     * float Sequence.t) Sequence.t)]; *)
  [%expect
    {|
    ((0 ((time 0) (bodies ()) (global_values ((g (Scalar 10)))) (cause (Init))))
     (5
      ((time 5)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06)) (v0 (Vector (0 0)))
           (x0 (Scalar 1)) (y0 (Scalar 1))))))
       (global_values ((g (Scalar 10)))) (cause ((BodyAdded (id 0)) Empty))))
     (10
      ((time 10)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06)) (v0 (Vector (2 2)))
           (x0 (Scalar 1)) (y0 (Scalar 1))))
         (1
          ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
           (v0 (Vector (-1 -1))) (x0 (Scalar 7)) (y0 (Scalar 5))))))
       (global_values ((g (Scalar 10))))
       (cause
        ((VelocityGiven (id 1) (v (-1 -1))) (VelocityGiven (id 0) (v (2 2)))
         (BodyAdded (id 1)) Empty))))
     (11
      ((time 11)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06))
           (v0 (Vector (1.9999646446609407 1.9999646446609407)))
           (x0 (Scalar 2.9999823223304705)) (y0 (Scalar 2.9999823223304705))))
         (1
          ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
           (v0 (Vector (-0.99999999646446613 -0.99999999646446613)))
           (x0 (Scalar 6.0000000017677673)) (y0 (Scalar 4.0000000017677673))))))
       (global_values ((g (Scalar 10)))) (cause (Empty Empty))))
     (11.043063513818197
      ((time 11.043063513818197)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06))
           (v0 (Vector (-2.77404813585298 -1.1110995224660645)))
           (x0 (Scalar 3.0861077946590911)) (y0 (Scalar 3.0861077946590911))))
         (1
          ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
           (v0 (Vector (1.3870056326821811 0.55554665451495555)))
           (x0 (Scalar 5.956936488105101)) (y0 (Scalar 3.956936488105101))))))
       (global_values ((g (Scalar 10)))) (cause ((Collision (id1 0) (id2 1))))))
     (13
      ((time 13)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06))
           (v0 (Vector (-2.7739573040803935 -1.1110631412880161)))
           (x0 (Scalar -2.3424393408112807)) (y0 (Scalar 0.91179219719343807))))
         (1
          ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
           (v0 (Vector (1.3870056235990136 0.55554665087681365)))
           (x0 (Scalar 8.6712184083529458)) (y0 (Scalar 5.0441060025418478))))))
       (global_values ((g (Scalar 10)))) (cause (Empty Empty))))
     (14
      ((time 14)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06)) (v0 (Vector (3 1)))
           (x0 (Scalar -5.1163734372471676)) (y0 (Scalar -0.1992616486528748))))
         (1
          ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
           (v0 (Vector (1.3870056189574895 0.555546649017713)))
           (x0 (Scalar 10.058224029631196)) (y0 (Scalar 5.5996526524891106))))))
       (global_values ((g (Scalar 10))))
       (cause ((VelocityGiven (id 0) (v (3 1))) Empty))))
     (17.2
      ((time 17.2)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06))
           (v0 (Vector (2.9998482106723121 0.99994940355743733)))
           (x0 (Scalar 4.4833836998285292)) (y0 (Scalar 3.0006573970390242))))
         (1
          ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
           (v0 (Vector (1.3870056041046128 0.55554664306859081)))
           (x0 (Scalar 14.496641986530559)) (y0 (Scalar 7.3774019198271965))))))
       (global_values ((g (Scalar 10)))) (cause (Empty Empty))))) |}];
  let _a = recv_and_print a { time = 25.; action = SC.Action.Empty } in
  (* let t = SC.Scene.t ~eps (snd @@ Map.max_elt_exn a) in print_s [%sexp (t : (int * int
     * float Sequence.t) Sequence.t)]; *)
  [%expect
    {|
    ((0 ((time 0) (bodies ()) (global_values ((g (Scalar 10)))) (cause (Init))))
     (5
      ((time 5)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06)) (v0 (Vector (0 0)))
           (x0 (Scalar 1)) (y0 (Scalar 1))))))
       (global_values ((g (Scalar 10)))) (cause ((BodyAdded (id 0)) Empty))))
     (10
      ((time 10)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06)) (v0 (Vector (2 2)))
           (x0 (Scalar 1)) (y0 (Scalar 1))))
         (1
          ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
           (v0 (Vector (-1 -1))) (x0 (Scalar 7)) (y0 (Scalar 5))))))
       (global_values ((g (Scalar 10))))
       (cause
        ((VelocityGiven (id 1) (v (-1 -1))) (VelocityGiven (id 0) (v (2 2)))
         (BodyAdded (id 1)) Empty))))
     (11
      ((time 11)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06))
           (v0 (Vector (1.9999646446609407 1.9999646446609407)))
           (x0 (Scalar 2.9999823223304705)) (y0 (Scalar 2.9999823223304705))))
         (1
          ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
           (v0 (Vector (-0.99999999646446613 -0.99999999646446613)))
           (x0 (Scalar 6.0000000017677673)) (y0 (Scalar 4.0000000017677673))))))
       (global_values ((g (Scalar 10)))) (cause (Empty Empty))))
     (11.043063513818197
      ((time 11.043063513818197)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06))
           (v0 (Vector (-2.77404813585298 -1.1110995224660645)))
           (x0 (Scalar 3.0861077946590911)) (y0 (Scalar 3.0861077946590911))))
         (1
          ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
           (v0 (Vector (1.3870056326821811 0.55554665451495555)))
           (x0 (Scalar 5.956936488105101)) (y0 (Scalar 3.956936488105101))))))
       (global_values ((g (Scalar 10)))) (cause ((Collision (id1 0) (id2 1))))))
     (13
      ((time 13)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06))
           (v0 (Vector (-2.7739573040803935 -1.1110631412880161)))
           (x0 (Scalar -2.3424393408112807)) (y0 (Scalar 0.91179219719343807))))
         (1
          ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
           (v0 (Vector (1.3870056235990136 0.55554665087681365)))
           (x0 (Scalar 8.6712184083529458)) (y0 (Scalar 5.0441060025418478))))))
       (global_values ((g (Scalar 10)))) (cause (Empty Empty))))
     (14
      ((time 14)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06)) (v0 (Vector (3 1)))
           (x0 (Scalar -5.1163734372471676)) (y0 (Scalar -0.1992616486528748))))
         (1
          ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
           (v0 (Vector (1.3870056189574895 0.555546649017713)))
           (x0 (Scalar 10.058224029631196)) (y0 (Scalar 5.5996526524891106))))))
       (global_values ((g (Scalar 10))))
       (cause ((VelocityGiven (id 0) (v (3 1))) Empty))))
     (17.2
      ((time 17.2)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06))
           (v0 (Vector (2.9998482106723121 0.99994940355743733)))
           (x0 (Scalar 4.4833836998285292)) (y0 (Scalar 3.0006573970390242))))
         (1
          ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
           (v0 (Vector (1.3870056041046128 0.55554664306859081)))
           (x0 (Scalar 14.496641986530559)) (y0 (Scalar 7.3774019198271965))))))
       (global_values ((g (Scalar 10)))) (cause (Empty Empty))))
     (22.133812081711948
      ((time 22.133812081711948)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06))
           (v0 (Vector (1.6936861035870081 1.7311768418394169)))
           (x0 (Scalar 19.283493711826672)) (y0 (Scalar 7.9340274010384038))))
         (1
          ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
           (v0 (Vector (2.0399696191189167 2.1384093349283395)))
           (x0 (Scalar 21.339866936970925)) (y0 (Scalar 10.118364636725946))))))
       (global_values ((g (Scalar 10)))) (cause ((Collision (id1 0) (id2 1))))))
     (25
      ((time 25)
       (bodies
        ((0
          ((m (Scalar 1)) (r (Scalar 1)) (mu (Scalar 5E-06))
           (v0 (Vector (1.6935858838239435 1.7310744036529402)))
           (x0 (Scalar 24.137772734963086)) (y0 (Scalar 12.89575874599238))))
         (1
          ((m (Scalar 2)) (r (Scalar 2)) (mu (Scalar 5E-10))
           (v0 (Vector (2.0399696092268953 2.1384093245589737)))
           (x0 (Scalar 27.186803198788049)) (y0 (Scalar 16.247447621991665))))))
       (global_values ((g (Scalar 10)))) (cause (Empty Empty))))) |}]
;; *)
