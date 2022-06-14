open Core
module S = Engine

let brownian_1 =
  Random.init 123;
  let left, right, top, bottom = 400., 600., 400., 600. in
  let square =
    Protocol.Defaults.Replays.square
      S.Point.{ x = left; y = top }
      S.Point.{ x = right; y = bottom }
  in
  let actions =
    Sequence.range 0 15
    |> Sequence.map ~f:(fun _ ->
           let open Float in
           let r = 10. in
           let id = S.Body.Id.next () in
           let add =
             S.Action.AddBody
               { id = Some id
               ; x0 = Random.float_range (left + r) (right - r)
               ; y0 = Random.float_range (top + r) (bottom - r)
               ; r
               ; mu = 0.
               ; m = 1.
               }
           in
           let give_v =
             S.Action.GiveVelocity
               { id
               ; v0 = Random.float_range (-100.) 100., Random.float_range (-100.) 100.
               }
           in
           [ add; give_v ])
    |> Sequence.bind ~f:Sequence.of_list
    |> Sequence.to_list
  in
  square @ actions |> Protocol.Defaults.Replays.unfold
;;

let%expect_test "" =
  let a = Lazy.force brownian_1 in
  print_s [%sexp (a : Engine.Model.t)];
  [%expect
    {|
    ((timeout (0.097649490379264084))
     (scenes
      ((0
        ((time 0)
         (bodies
          ((4
            ((id 4)
             (values
              ((v0_x 0.86271078967411086) (v0_y 19.716205904959949) (m 1)
               (r 10) (mu 0) (x0 427.7327025816291) (y0 410.8731567269499)))
             (rules (rules1_0 rules1_1))))
           (5
            ((id 5)
             (values
              ((v0_x 81.736658375857672) (v0_y 86.746712682729338) (m 1)
               (r 10) (mu 0) (x0 424.01846153998747) (y0 507.72981437345788)))
             (rules (rules1_0 rules1_1))))
           (6
            ((id 6)
             (values
              ((v0_x 95.852781857428567) (v0_y 80.164130214692989) (m 1)
               (r 10) (mu 0) (x0 511.16395664487993) (y0 454.68373729797713)))
             (rules (rules1_0 rules1_1))))
           (7
            ((id 7)
             (values
              ((v0_x 89.052596443201168) (v0_y 89.227558878377863) (m 1)
               (r 10) (mu 0) (x0 478.289187023875) (y0 494.76781851552272)))
             (rules (rules1_0 rules1_1))))
           (8
            ((id 8)
             (values
              ((v0_x 45.746890178406062) (v0_y -51.742007910759838) (m 1)
               (r 10) (mu 0) (x0 487.99174927231167) (y0 482.09101695729254)))
             (rules (rules1_0 rules1_1))))
           (9
            ((id 9)
             (values
              ((v0_x -70.303234441621157) (v0_y 0.056963218149206796) (m 1)
               (r 10) (mu 0) (x0 479.47322517677304) (y0 497.77954130594941)))
             (rules (rules1_0 rules1_1))))
           (10
            ((id 10)
             (values
              ((v0_x -4.453993382294243) (v0_y 29.758713432305768) (m 1)
               (r 10) (mu 0) (x0 542.91588368371879) (y0 551.71588572974224)))
             (rules (rules1_0 rules1_1))))
           (11
            ((id 11)
             (values
              ((v0_x 40.306986579592632) (v0_y 93.7478943811206) (m 1) (r 10)
               (mu 0) (x0 510.69929211029961) (y0 438.94058675099257)))
             (rules (rules1_0 rules1_1))))
           (12
            ((id 12)
             (values
              ((v0_x -9.77745919722733) (v0_y 54.647713187155034) (m 1) (r 10)
               (mu 0) (x0 545.39242067561509) (y0 514.22261429002583)))
             (rules (rules1_0 rules1_1))))
           (13
            ((id 13)
             (values
              ((v0_x 13.985886448574774) (v0_y -55.37645958343419) (m 1)
               (r 10) (mu 0) (x0 577.26895055792966) (y0 421.40130336625805)))
             (rules (rules1_0 rules1_1))))
           (14
            ((id 14)
             (values
              ((v0_x -25.134243364819696) (v0_y 58.470190427253272) (m 1)
               (r 10) (mu 0) (x0 465.65072596185382) (y0 569.62070100613278)))
             (rules (rules1_0 rules1_1))))
           (15
            ((id 15)
             (values
              ((v0_x 79.50475987610767) (v0_y 92.404730284592034) (m 1) (r 10)
               (mu 0) (x0 551.6798605012624) (y0 536.62450304350966)))
             (rules (rules1_0 rules1_1))))
           (16
            ((id 16)
             (values
              ((v0_x 17.435810081585586) (v0_y 45.037624663398276) (m 1)
               (r 10) (mu 0) (x0 432.28503471693551) (y0 442.48121322319111)))
             (rules (rules1_0 rules1_1))))
           (17
            ((id 17)
             (values
              ((v0_x 46.805198499496925) (v0_y -52.838994586484276) (m 1)
               (r 10) (mu 0) (x0 518.82655460404681) (y0 565.76523335009551)))
             (rules (rules1_0 rules1_1))))
           (18
            ((id 18)
             (values
              ((v0_x 0.94714408290828089) (v0_y 74.513907127285449) (m 1)
               (r 10) (mu 0) (x0 435.13704109597023) (y0 517.22696014545977)))
             (rules (rules1_0 rules1_1))))))
         (points ())
         (lines
          (((p1 ((x 400) (y 400))) (p2 ((x 400) (y 600))) (kind Segment))
           ((p1 ((x 400) (y 400))) (p2 ((x 600) (y 400))) (kind Segment))
           ((p1 ((x 600) (y 600))) (p2 ((x 400) (y 600))) (kind Segment))
           ((p1 ((x 600) (y 600))) (p2 ((x 600) (y 400))) (kind Segment))))
         (global_values ((g 1)))
         (cause
          ((Action
            ((time 0)
             (action
              (GiveVelocity (id 18)
               (v0 (0.94714408290828089 74.513907127285449))))
             (until ((timespan ()) (quantity (1))))))
           Init
           (Action
            ((time 0)
             (action
              (AddBody (id (4)) (x0 427.7327025816291) (y0 410.8731567269499)
               (r 10) (mu 0) (m 1)))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (AddBody (id (5)) (x0 424.01846153998747) (y0 507.72981437345788)
               (r 10) (mu 0) (m 1)))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (AddBody (id (6)) (x0 511.16395664487993) (y0 454.68373729797713)
               (r 10) (mu 0) (m 1)))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (AddBody (id (7)) (x0 478.289187023875) (y0 494.76781851552272)
               (r 10) (mu 0) (m 1)))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (AddBody (id (8)) (x0 487.99174927231167) (y0 482.09101695729254)
               (r 10) (mu 0) (m 1)))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (AddBody (id (9)) (x0 479.47322517677304) (y0 497.77954130594941)
               (r 10) (mu 0) (m 1)))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (AddBody (id (10)) (x0 542.91588368371879) (y0 551.71588572974224)
               (r 10) (mu 0) (m 1)))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (AddBody (id (11)) (x0 510.69929211029961) (y0 438.94058675099257)
               (r 10) (mu 0) (m 1)))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (AddBody (id (12)) (x0 545.39242067561509) (y0 514.22261429002583)
               (r 10) (mu 0) (m 1)))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (AddBody (id (13)) (x0 577.26895055792966) (y0 421.40130336625805)
               (r 10) (mu 0) (m 1)))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (AddBody (id (14)) (x0 465.65072596185382) (y0 569.62070100613278)
               (r 10) (mu 0) (m 1)))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (AddBody (id (15)) (x0 551.6798605012624) (y0 536.62450304350966)
               (r 10) (mu 0) (m 1)))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (AddBody (id (16)) (x0 432.28503471693551) (y0 442.48121322319111)
               (r 10) (mu 0) (m 1)))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (AddBody (id (17)) (x0 518.82655460404681) (y0 565.76523335009551)
               (r 10) (mu 0) (m 1)))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (AddBody (id (18)) (x0 435.13704109597023) (y0 517.22696014545977)
               (r 10) (mu 0) (m 1)))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (AddLine
               ((p1 ((x 400) (y 400))) (p2 ((x 400) (y 600))) (kind Segment))))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (AddLine
               ((p1 ((x 400) (y 400))) (p2 ((x 600) (y 400))) (kind Segment))))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (AddLine
               ((p1 ((x 600) (y 600))) (p2 ((x 400) (y 600))) (kind Segment))))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (AddLine
               ((p1 ((x 600) (y 600))) (p2 ((x 600) (y 400))) (kind Segment))))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (GiveVelocity (id 4) (v0 (0.86271078967411086 19.716205904959949))))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (GiveVelocity (id 5) (v0 (81.736658375857672 86.746712682729338))))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (GiveVelocity (id 6) (v0 (95.852781857428567 80.164130214692989))))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (GiveVelocity (id 7) (v0 (89.052596443201168 89.227558878377863))))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (GiveVelocity (id 8) (v0 (45.746890178406062 -51.742007910759838))))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (GiveVelocity (id 9)
               (v0 (-70.303234441621157 0.056963218149206796))))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (GiveVelocity (id 10) (v0 (-4.453993382294243 29.758713432305768))))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (GiveVelocity (id 11) (v0 (40.306986579592632 93.7478943811206))))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (GiveVelocity (id 12) (v0 (-9.77745919722733 54.647713187155034))))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (GiveVelocity (id 13) (v0 (13.985886448574774 -55.37645958343419))))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (GiveVelocity (id 14)
               (v0 (-25.134243364819696 58.470190427253272))))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (GiveVelocity (id 15) (v0 (79.50475987610767 92.404730284592034))))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (GiveVelocity (id 16) (v0 (17.435810081585586 45.037624663398276))))
             (until ((timespan ()) (quantity (1))))))
           (Action
            ((time 0)
             (action
              (GiveVelocity (id 17)
               (v0 (46.805198499496925 -52.838994586484276))))
             (until ((timespan ()) (quantity (1))))))))))
       (0.097649490379264084
        ((time 0.097649490379264084)
         (bodies
          ((4
            ((id 4)
             (values
              ((v0_x 0.86271078967411086) (v0_y 19.716205904959949) (m 1)
               (r 10) (mu 0) (x0 427.81694585058545) (y0 412.7984341857819)))
             (rules (rules1_0 rules1_1))))
           (5
            ((id 5)
             (values
              ((v0_x 81.736658375857672) (v0_y 86.746712682729338) (m 1)
               (r 10) (mu 0) (x0 432.000004575694) (y0 516.20058665900285)))
             (rules (rules1_0 rules1_1))))
           (6
            ((id 6)
             (values
              ((v0_x 95.852781857428567) (v0_y 80.164130214692989) (m 1)
               (r 10) (mu 0) (x0 520.52393194469255) (y0 462.51172376013886)))
             (rules (rules1_0 rules1_1))))
           (7
            ((id 7)
             (values
              ((v0_x 89.052596443201168) (v0_y 89.227558878377863) (m 1)
               (r 10) (mu 0) (x0 486.98512768350383) (y0 503.4808441677821)))
             (rules (rules1_0 rules1_1))))
           (8
            ((id 8)
             (values
              ((v0_x 45.746890178406062) (v0_y -51.742007910759838) (m 1)
               (r 10) (mu 0) (x0 492.45890978466917) (y0 477.038436253607)))
             (rules (rules1_0 rules1_1))))
           (9
            ((id 9)
             (values
              ((v0_x -70.303234441621157) (v0_y 0.056963218149206796) (m 1)
               (r 10) (mu 0) (x0 472.60815016153481) (y0 497.785103735172)))
             (rules (rules1_0 rules1_1))))
           (10
            ((id 10)
             (values
              ((v0_x 65.7970158510537) (v0_y 7.731626502425069) (m 1) (r 10)
               (mu 0) (x0 542.48095349978519) (y0 554.62180893074947)))
             (rules (rules1_0 rules1_1))))
           (11
            ((id 11)
             (values
              ((v0_x 40.306986579592632) (v0_y 93.7478943811206) (m 1) (r 10)
               (mu 0) (x0 514.63524880852071) (y0 448.09502086143806)))
             (rules (rules1_0 rules1_1))))
           (12
            ((id 12)
             (values
              ((v0_x -9.77745919722733) (v0_y 54.647713187155034) (m 1) (r 10)
               (mu 0) (x0 544.43765676780174) (y0 519.55893563314373)))
             (rules (rules1_0 rules1_1))))
           (13
            ((id 13)
             (values
              ((v0_x 13.985886448574774) (v0_y -55.37645958343419) (m 1)
               (r 10) (mu 0) (x0 578.63466524213527) (y0 415.99382030892781)))
             (rules (rules1_0 rules1_1))))
           (14
            ((id 14)
             (values
              ((v0_x -25.134243364819696) (v0_y 58.470190427253272) (m 1)
               (r 10) (mu 0) (x0 463.19637990621078) (y0 575.33028530373258)))
             (rules (rules1_0 rules1_1))))
           (15
            ((id 15)
             (values
              ((v0_x 79.50475987610767) (v0_y 92.404730284592034) (m 1) (r 10)
               (mu 0) (x0 559.44345978589013) (y0 545.64777786443346)))
             (rules (rules1_0 rules1_1))))
           (16
            ((id 16)
             (values
              ((v0_x 17.435810081585586) (v0_y 45.037624663398276) (m 1)
               (r 10) (mu 0) (x0 433.987632685752) (y0 446.87911431946452)))
             (rules (rules1_0 rules1_1))))
           (17
            ((id 17)
             (values
              ((v0_x -23.445810733851019) (v0_y -30.811907656603577) (m 1)
               (r 10) (mu 0) (x0 523.39705838462294) (y0 560.60553245657263)))
             (rules (rules1_0 rules1_1))))
           (18
            ((id 18)
             (values
              ((v0_x 0.94714408290828089) (v0_y 74.513907127285449) (m 1)
               (r 10) (mu 0) (x0 435.229529232982) (y0 524.503205202607)))
             (rules (rules1_0 rules1_1))))))
         (points ())
         (lines
          (((p1 ((x 400) (y 400))) (p2 ((x 400) (y 600))) (kind Segment))
           ((p1 ((x 400) (y 400))) (p2 ((x 600) (y 400))) (kind Segment))
           ((p1 ((x 600) (y 600))) (p2 ((x 400) (y 600))) (kind Segment))
           ((p1 ((x 600) (y 600))) (p2 ((x 600) (y 400))) (kind Segment))))
         (global_values ((g 1)))
         (cause ((Collision (Collision (id1 10) (id2 17)))))))))) |}]
;;
