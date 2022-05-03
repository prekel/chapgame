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
         ; action = AddBody { id = id2; x0 = 10.; y0 = 2.; r = 1.; mu = 0.; m = m2 }
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
  let model = model_pi 1e2 in
  print_s [%sexp (model : S.Model.t)];
  print_s [%sexp (model_to_pi model : int)];
  [%expect {|
    ((scenes
      ((0
        ((time 0)
         (bodies
          ((0
            ((id 0)
             (values ((v0_x -1) (v0_y 0) (m 1) (r 1) (mu 0) (x0 5) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values ((v0_x -1) (v0_y 0) (m 100) (r 1) (mu 0) (x0 10) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause
          (Empty Init (VelocityGiven (id 0) (v (-1 0)))
           (VelocityGiven (id 1) (v (-1 0))) (BodyAdded (id 0))
           (BodyAdded (id 1)) (PointAdded ((x 0) (y 2)))))))
       (4
        ((time 4)
         (bodies
          ((0
            ((id 0) (values ((v0_x 1) (v0_y 0) (m 1) (r 1) (mu 0) (x0 1) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values ((v0_x -1) (v0_y 0) (m 100) (r 1) (mu 0) (x0 6) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((CollisionWithPoint (id 0) (point ((x 0) (y 2))))))))
       (5.5
        ((time 5.5)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x -2.9603960396039604) (v0_y 0) (m 1) (r 1) (mu 0) (x0 2.5)
               (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x -0.96039603960396036) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 4.5) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((Collision (id1 0) (id2 1))))))
       (6.0066889632107028
        ((time 6.0066889632107028)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x 2.9603960396039604) (v0_y 0) (m 1) (r 1) (mu 0) (x0 1)
               (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x -0.96039603960396036) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 4.0133779264214047) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((CollisionWithPoint (id 0) (point ((x 0) (y 2))))))))
       (6.2651515151515156
        ((time 6.2651515151515156)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x -4.8035486716988531) (v0_y 0) (m 1) (r 1) (mu 0)
               (x0 1.7651515151515151) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x -0.88275659249093219) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.7651515151515151) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((Collision (id1 0) (id2 1))))))
       (6.4244403175445406
        ((time 6.4244403175445406)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x 4.8035486716988531) (v0_y 0) (m 1) (r 1) (mu 0) (x0 1)
               (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x -0.88275659249093219) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.6245382747290873) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((CollisionWithPoint (id 0) (point ((x 0) (y 2))))))))
       (6.5342723166568986
        ((time 6.5342723166568986)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x -6.4564617524393357) (v0_y 0) (m 1) (r 1) (mu 0)
               (x0 1.5275833534461953) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x -0.7701564882495503) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.5275833534461953) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((Collision (id1 0) (id2 1))))))
       (6.6159863225126392
        ((time 6.6159863225126392)
         (bodies  
          ((0
            ((id 0)
             (values
              ((v0_x 6.4564617524393357) (v0_y 0) (m 1) (r 1) (mu 0) (x0 1)
               (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x -0.7701564882495503) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.4646507816555348) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((CollisionWithPoint (id 0) (point ((x 0) (y 2))))))))
       (6.6802834482469633
        ((time 6.6802834482469633)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x -7.8536733776376666) (v0_y 0) (m 1) (r 1) (mu 0)
               (x0 1.4151319330954477) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x -0.62705513694878023) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.4151319330954477) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((Collision (id1 0) (id2 1))))))
       (6.7331417622019956
        ((time 6.7331417622019956)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x 7.8536733776376666) (v0_y 0) (m 1) (r 1) (mu 0) (x0 1)
               (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x -0.62705513694878023) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.3819868557994934) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((CollisionWithPoint (id 0) (point ((x 0) (y 2))))))))
       (6.7781835124646639
        ((time 6.7781835124646639)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x -8.9398484334246042) (v0_y 0) (m 1) (r 1) (mu 0)
               (x0 1.3537431949201231) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x -0.4591199188381575) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.3537431949201233) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((Collision (id1 0) (id2 1))))))
       (6.8177527733480519
        ((time 6.8177527733480519)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x 8.9398484334246042) (v0_y 0) (m 1) (r 1) (mu 0) (x0 1)
               (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x -0.4591199188381575) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.3355761590748565) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((CollisionWithPoint (id 0) (point ((x 0) (y 2))))))))
       (6.8534562831906047
        ((time 6.8534562831906047)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x -9.6719700859075957) (v0_y 0) (m 1) (r 1) (mu 0)
               (x0 1.3191839665337062) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x -0.27300173364483549) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.3191839665337062) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((Collision (id1 0) (id2 1))))))
       (6.8864572089274168
        ((time 6.8864572089274168)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x 9.6719700859075957) (v0_y 0) (m 1) (r 1) (mu 0) (x0 1)
               (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x -0.27300173364483549) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.310174656595672) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((CollisionWithPoint (id 0) (point ((x 0) (y 2))))))))
       (6.9176463024938606
        ((time 6.9176463024938606)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x -10.021043418156626) (v0_y 0) (m 1) (r 1) (mu 0)
               (x0 1.3016599799812218) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x -0.076071598604193269) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.3016599799812218) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((Collision (id1 0) (id2 1))))))
       (6.9477489542231865
        ((time 6.9477489542231865)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x 10.021043418156626) (v0_y 0) (m 1) (r 1) (mu 0) (x0 1)
               (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x -0.076071598604193269) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.2993700231419467) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((CollisionWithPoint (id 0) (point ((x 0) (y 2))))))))
       (6.9773980195893834
        ((time 6.9773980195893834)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x -9.9732437437459858) (v0_y 0) (m 1) (r 1) (mu 0)
               (x0 1.2971145713424201) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x 0.12387127301483286) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.29711457134242) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((Collision (id1 0) (id2 1))))))
       (7.0071891867338474
        ((time 7.0071891867338474)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x 9.9732437437459858) (v0_y 0) (m 1) (r 1) (mu 0) (x0 1)
               (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x 0.12387127301483286) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.3008048411412028) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((CollisionWithPoint (id 0) (point ((x 0) (y 2))))))))
       (7.03772969497791
        ((time 7.03772969497791)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x -9.53046411908798) (v0_y 0) (m 1) (r 1) (mu 0)
               (x0 1.3045879327759147) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x 0.31890835164317255) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.3045879327759149) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((Collision (id1 0) (id2 1))))))
       (7.0696890968464787
        ((time 7.0696890968464787)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x 9.53046411908798) (v0_y 0) (m 1) (r 1) (mu 0)
               (x0 1.0000000000000002) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x 0.31890835164317255) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.3147800529453222) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((CollisionWithPoint (id 0) (point ((x 0) (y 2))))))))
       (7.1038613974754137
        ((time 7.1038613974754137)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x -8.71024037090174) (v0_y 0) (m 1) (r 1) (mu 0)
               (x0 1.3256778850107502) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x 0.50131539654306978) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.3256778850107507) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((Collision (id1 0) (id2 1))))))
       (7.1412516268080894
        ((time 7.1412516268080894)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x 8.71024037090174) (v0_y 0) (m 1) (r 1) (mu 0) (x0 1) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x 0.50131539654306978) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.3444221826554976) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((CollisionWithPoint (id 0) (point ((x 0) (y 2))))))))
       (7.1832086654862755
        ((time 7.1832086654862755)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x -7.5450566080263179) (v0_y 0) (m 1) (r 1) (mu 0)
               (x0 1.3654558921382252) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x 0.66386836633235036) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.3654558921382254) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((Collision (id1 0) (id2 1))))))
       (7.2316451333783069
        ((time 7.2316451333783069)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x 7.5450566080263179) (v0_y 0) (m 1) (r 1) (mu 0) (x0 1)
               (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x 0.66386836633235036) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.3976113309486173) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((CollisionWithPoint (id 0) (point ((x 0) (y 2))))))))
       (7.28942749842675
        ((time 7.28942749842675)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x -6.0810587220607477) (v0_y 0) (m 1) (r 1) (mu 0)
               (x0 1.4359712152361468) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x 0.800129519633221) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.435971215236147) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((Collision (id1 0) (id2 1))))))
       (7.3611208063131777
        ((time 7.3611208063131777)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x 6.0810587220607477) (v0_y 0) (m 1) (r 1) (mu 0) (x0 1)
               (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x 0.800129519633221) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.4933351472362308) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((CollisionWithPoint (id 0) (point ((x 0) (y 2))))))))
       (7.4545390530507891
        ((time 7.4545390530507891)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x -4.3762268273006928) (v0_y 0) (m 1) (r 1) (mu 0)
               (x0 1.5680818441233737) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x 0.90470237512683538) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.5680818441233733) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((Collision (id1 0) (id2 1))))))
       (7.5843499304439455
        ((time 7.5843499304439455)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x 4.3762268273006928) (v0_y 0) (m 1) (r 1) (mu 0) (x0 1)
               (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x 0.90470237512683538) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.6855220532182607) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((CollisionWithPoint (id 0) (point ((x 0) (y 2))))))))
       (7.7818199649093485
        ((time 7.7818199649093485)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x -2.4980790185881343) (v0_y 0) (m 1) (r 1) (mu 0)
               (x0 1.8641736624154888) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x 0.97344543358572366) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 3.8641736624154888) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((Collision (id1 0) (id2 1))))))
       (8.1277552439718583
        ((time 8.1277552439718583)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x 2.4980790185881343) (v0_y 0) (m 1) (r 1) (mu 0) (x0 1)
               (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x 0.97344543358572366) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 4.2009227801350928) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((CollisionWithPoint (id 0) (point ((x 0) (y 2))))))))
       (8.9154348490575579
        ((time 8.9154348490575579)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x -0.52099738735723333) (v0_y 0) (m 1) (r 1) (mu 0)
               (x0 2.9676858948343732) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x 1.0036361976451773) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 4.9676858948343732) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((Collision (id1 0) (id2 1))))))
       (12.692202146904267
        ((time 12.692202146904267)
         (bodies
          ((0
            ((id 0)
             (values
              ((v0_x 0.52099738735723333) (v0_y 0) (m 1) (r 1) (mu 0) (x0 1)
               (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))
           (1
            ((id 1)
             (values
              ((v0_x 1.0036361976451773) (v0_y 0) (m 100) (r 1) (mu 0)
               (x0 8.7581862650358957) (y0 2)))
             (rules ("rules1 - 0" "rules1 - 1"))))))
         (points (((x 0) (y 2)))) (lines ()) (global_values ((g 10)))
         (cause ((CollisionWithPoint (id 0) (point ((x 0) (y 2))))))))))
     (timeout ()))
    31 |}]
;;

let%expect_test "pi" =
  print_s [%sexp (1e2 |> model_pi |> model_to_pi : int)];
  print_s [%sexp (1e4 |> model_pi |> model_to_pi : int)];
  print_s [%sexp (1e6 |> model_pi |> model_to_pi : int)];
  print_s [%sexp (1e8 |> model_pi |> model_to_pi : int)];
  print_s [%sexp (1e10 |> model_pi |> model_to_pi : int)];
  [%expect {|
    31
    314
    3141
    31415
    314159 |}]
;;
