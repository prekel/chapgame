open Core
module S = Chapgame.Scene.Make ((val Chapgame.Utils.make_consts ~eps:1e-6))

let model_pi m2 =
  let id1 = S.Figure2.Id.next () in
  let id2 = S.Figure2.Id.next () in
  S.Model.init ~g:10.
  |> S.Engine.recv
       ~action:
         { time = 0.
         ; action = AddBody { id = id1; x0 = 350.; y0 = 200.; r = 100.; mu = 0.; m = 1. }
         ; timeout = Some 0.
         }
  |> S.Engine.recv
       ~action:
         { time = 0.
         ; action = AddBody { id = id2; x0 = 700.; y0 = 200.; r = 100.; mu = 0.; m = m2 }
         ; timeout = Some 0.
         }
  |> S.Engine.recv
       ~action:{ time = 0.; action = AddPoint { x = 0.; y = 200. }; timeout = Some 0. }
  |> S.Engine.recv
       ~action:
         { time = 0.
         ; action = GiveVelocity { id = id1; v0 = -100., 0. }
         ; timeout = Some 0.
         }
  |> S.Engine.recv
       ~action:
         { time = 0.
         ; action = GiveVelocity { id = id2; v0 = -100., 0. }
         ; timeout = Some 0.
         }
  |> S.Engine.recv ~action:{ time = 0.; action = Empty; timeout = None }
;;

let model_to_pi S.Model.{ scenes; _ } =
  scenes
  |> S.Model.Scenes.to_sequence
  |> Sequence.bind ~f:(fun (_, s) -> Sequence.of_list s.cause)
  |> Sequence.sum
       (module Int)
       ~f:(function
         | Collision _ | CollisionWithPoint _ -> 1
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
    3.141592653589793
    31
    314
    3141
    31415 |}]
;;
