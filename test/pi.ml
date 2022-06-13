open Core
module S = Engine

let%expect_test "sample" =
  let id1 = S.Body.Id.next () in
  let id2 = S.Body.Id.next () in
  S.Model.init ~g:1.
  |> S.recv
       ~action:
         { time = 0.
         ; action =
             AddBody { id = Some id1; x0 = 350.; y0 = 200.; r = 100.; mu = 1.; m = 10. }
         ; until = { timespan = Some 0.; quantity = None }
         }
  |> S.recv
       ~action:
         { time = 0.
         ; action =
             AddBody { id = Some id2; x0 = 700.; y0 = 200.; r = 100.; mu = 1.; m = 10. }
         ; until = { timespan = Some 0.; quantity = None }
         }
  |> S.recv
       ~action:
         { time = 0.
         ; action = GiveVelocity { id = id2; v0 = -100., 0. }
         ; until = { timespan = None; quantity = None }
         }
  |> [%sexp_of: S.Model.t]
  |> print_s;
  [%expect
    {|
    ((timeout ())
     (scenes
      ((0
        ((time 0)
         (bodies
          ((0
            ((id 0)
             (values ((v0_x 0) (v0_y 0) (m 10) (r 100) (mu 1) (x0 350) (y0 200)))
             (rules (rules0_0))))
           (1
            ((id 1)
             (values
              ((v0_x -100) (v0_y 0) (m 10) (r 100) (mu 1) (x0 700) (y0 200)))
             (rules (rules1_0 rules1_1))))))
         (points ()) (lines ()) (global_values ((g 1)))
         (cause
          ((Action
            ((time 0) (action (GiveVelocity (id 1) (v0 (-100 0))))
             (until ((timespan ()) (quantity ())))))
           Init
           (Action
            ((time 0)
             (action (AddBody (id (0)) (x0 350) (y0 200) (r 100) (mu 1) (m 10)))
             (until ((timespan (0)) (quantity ())))))
           (Action
            ((time 0)
             (action (AddBody (id (1)) (x0 700) (y0 200) (r 100) (mu 1) (m 10)))
             (until ((timespan (0)) (quantity ())))))))))
       (1.511421982049427
        ((time 1.511421982049427)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x -98.488578017950573) (v0_y 0) (m 10) (r 100) (mu 1)
               (x0 350) (y0 200)))
             (rules (rules1_0 rules1_1))))
           (1
            ((id 1)
             (values
              ((v0_x 0) (v0_y 0) (m 10) (r 100) (mu 1) (x0 549.99999999896841)
               (y0 200)))
             (rules (rules1_0 rules1_1))))))
         (points ()) (lines ()) (global_values ((g 1)))
         (cause ((Collision (Collision (id1 0) (id2 1)))))))))) |}]
;;

let model_pi m2 =
  let id1 = S.Body.Id.next () in
  let id2 = S.Body.Id.next () in
  S.Model.init ~g:1.
  |> S.recv
       ~action:
         { time = 0.
         ; action =
             AddBody { id = Some id1; x0 = 350.; y0 = 200.; r = 100.; mu = 0.; m = 1. }
         ; until = { timespan = Some 0.; quantity = None }
         }
  |> S.recv
       ~action:
         { time = 0.
         ; action =
             AddBody { id = Some id2; x0 = 700.; y0 = 200.; r = 100.; mu = 0.; m = m2 }
         ; until = { timespan = Some 0.; quantity = None }
         }
  |> S.recv
       ~action:
         { time = 0.
         ; action = AddPoint { x = 0.; y = 200. }
         ; until = { timespan = Some 0.; quantity = None }
         }
  |> S.recv
       ~action:
         { time = 0.
         ; action = GiveVelocity { id = id1; v0 = -100., 0. }
         ; until = { timespan = Some 0.; quantity = None }
         }
  |> S.recv
       ~action:
         { time = 0.
         ; action = GiveVelocity { id = id2; v0 = -100., 0. }
         ; until = { timespan = Some 0.; quantity = None }
         }
  |> S.update ~action:(`Prolong S.Action.{ timespan = None; quantity = None })
;;

let model_to_pi S.Model.{ scenes; _ } =
  scenes
  |> S.Scenes.to_sequence
  |> Sequence.bind ~f:(fun (_, s) -> Sequence.of_list s.cause)
  |> Sequence.sum
       (module Int)
       ~f:(function
         | `Collision _ -> 1
         | _ -> 0)
;;

let%expect_test "3.1" =
  let model = model_pi 1e2 in
  (* print_s [%sexp (model : S.Model.t)]; *)
  print_s [%sexp (model_to_pi model : int)];
  [%expect {|
    31 |}]
;;

let%expect_test "pi" =
  print_s [%sexp (Float.pi : float)];
  print_s [%sexp (1e2 |> model_pi |> model_to_pi : int)];
  print_s [%sexp (1e4 |> model_pi |> model_to_pi : int)];
  print_s [%sexp (1e6 |> model_pi |> model_to_pi : int)];
  print_s [%sexp (1e8 |> model_pi |> model_to_pi : int)];
  [%expect {|
    3.1415926535897931
    31
    314
    3141
    31415 |}]
;;
