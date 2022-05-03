open Core
module S = Chapgame.Scene.Make ((val Chapgame.Utils.make_consts ~eps:1e-6))

let model_pi m2 =
  let id1 = S.Figure2.Id.next () in
  let id2 = S.Figure2.Id.next () in
  S.Model.init ~g:10.
  |> S.Engine.recv
       ~action:
         { time = 0.
         ; action = AddBody { id = id1; x0 = 5.; y0 = 2.; r = 1.; mu = 0.; m = 1. }
         ; timeout = Some 0.
         }
  |> S.Engine.recv
       ~action:
         { time = 0.
         ; action =
             AddBody
               { id = id2
               ; x0 = 10.
               ; y0 = 2.
               ; r = 1.
               ; mu = 0.
               ; m = 10. ** Int.to_float m2
               }
         ; timeout = Some 0.
         }
  |> S.Engine.recv
       ~action:{ time = 0.; action = AddPoint { x = 0.; y = 2. }; timeout = Some 0. }
  |> S.Engine.recv
       ~action:
         { time = 0.
         ; action = GiveVelocity { id = id1; v0 = -1., 0. }
         ; timeout = Some 0.
         }
  |> S.Engine.recv
       ~action:
         { time = 0.
         ; action = GiveVelocity { id = id2; v0 = -1., 0. }
         ; timeout = Some 0.
         }
  |> S.Engine.recv ~action:{ time = 0.; action = Empty; timeout = None }
;;

let model_to_pi S.Model.{ scenes; _ } = (S.Model.Scenes.to_map scenes |> Map.length) - 1

let%expect_test "3.1" =
  let model = model_pi 2 in
  (* print_s [%sexp (model : S.Model.t)]; *)
  print_s [%sexp (model_to_pi model : int)];
  [%expect
    {|
    31 |}]
;;

let%expect_test "pi" =
  print_s [%sexp (2 |> model_pi |> model_to_pi : int)];
  print_s [%sexp (4 |> model_pi |> model_to_pi : int)];
  print_s [%sexp (6 |> model_pi |> model_to_pi : int)];
  print_s [%sexp (8 |> model_pi |> model_to_pi : int)];
  [%expect {|
    31
    314
    3141
    31415 |}]
;;
