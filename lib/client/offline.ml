open Core
open Bonsai_web
open Bonsai.Let_syntax

module Make
    (C : Engine.Module_types.CONSTS with module N = Float)
    (S : module type of Engine.Scene.Make (C))
    (SC : module type of Scene.Make (C) (S)) =
    struct
      let route = [ "offline" ], []

      let component =
        let%sub state, dispatch =
          Bonsai.state_machine0
            [%here]
            (module S.Model)
            (module struct
              type t =
                [ `Action of S.Action.t
                | `Replace of S.Model.t
                ]
              [@@deriving sexp, equal]
            end)
            ~default_model:
              (S.Model.init ~g:10.
              |> S.Engine.recv
                   ~action:
                     { time = 0.
                     ; action =
                         S.Action.AddBody
                           { id = Some (S.Body.Id.next ())
                           ; x0 = 425.
                           ; y0 = 275.
                           ; r = 2.
                           ; mu = 5.
                           ; m = Float.(pi * 2. * 2.)
                           }
                     ; timeout = None
                     }
              |> S.Engine.recv
                   ~action:
                     { time = 0.
                     ; action =
                         S.Action.AddBody
                           { id = Some (S.Body.Id.next ())
                           ; x0 = 450.
                           ; y0 = 250.
                           ; r = 10.
                           ; mu = 1.
                           ; m = Float.(pi * 10. * 10.)
                           }
                     ; timeout = None
                     }
              |> S.Engine.recv
                   ~action:
                     { time = 0.
                     ; action =
                         S.Action.AddBody
                           { id = Some (S.Body.Id.next ())
                           ; x0 = 600.
                           ; y0 = 600.
                           ; r = 50.
                           ; mu = 2.
                           ; m = Float.(pi * 50. * 50.)
                           }
                     ; timeout = None
                     }
              |> S.Engine.recv
                   ~action:
                     { time = 0.
                     ; action =
                         S.Action.AddBody
                           { id = Some (S.Body.Id.next ())
                           ; x0 = 500.
                           ; y0 = 500.
                           ; r = 60.
                           ; mu = 3.
                           ; m = Float.(pi * 60. * 60.)
                           }
                     ; timeout = None
                     }
              |> S.Engine.recv
                   ~action:
                     { time = 0.
                     ; action = S.Action.AddPoint { x = 400.; y = 200. }
                     ; timeout = None
                     }
              |> S.Engine.recv
                   ~action:
                     { time = 0.
                     ; action = S.Action.AddPoint { x = 1100.; y = 100. }
                     ; timeout = None
                     }
              |> S.Engine.recv
                   ~action:
                     { time = 0.
                     ; action = S.Action.AddPoint { x = 100.; y = 700. }
                     ; timeout = None
                     }
              |> S.Engine.recv
                   ~action:
                     { time = 0.
                     ; action = S.Action.AddPoint { x = 700.; y = 700. }
                     ; timeout = None
                     }
              |> S.Engine.recv
                   ~action:
                     { time = 0.
                     ; action = S.Action.AddPoint { x = 650.; y = 325. }
                     ; timeout = None
                     }
              |> S.Engine.recv
                   ~action:
                     { time = 0.
                     ; action = S.Action.AddPoint { x = 600.; y = 400. }
                     ; timeout = None
                     }
              |> S.Engine.recv
                   ~action:
                     { time = 0.
                     ; action = S.Action.AddPoint { x = 700.; y = 450. }
                     ; timeout = None
                     }
              |> S.Engine.recv
                   ~action:
                     { time = 0.
                     ; action =
                         S.Action.AddLine
                           (S.LineSegmentRay.of_points
                              ~p1:{ x = 650.; y = 325. }
                              ~p2:{ x = 600.; y = 400. }
                              ~kind:`Segment)
                     ; timeout = None
                     }
              |> S.Engine.recv
                   ~action:
                     { time = 0.
                     ; action =
                         S.Action.AddLine
                           (S.LineSegmentRay.of_points
                              ~p1:{ x = 600.; y = 400. }
                              ~p2:{ x = 700.; y = 450. }
                              ~kind:`Segment)
                     ; timeout = None
                     }
              |> S.Engine.recv
                   ~action:
                     { time = 0.
                     ; action =
                         S.Action.AddLine
                           (S.LineSegmentRay.of_points
                              ~p1:{ x = 400.; y = 200. }
                              ~p2:{ x = 1100.; y = 100. }
                              ~kind:`Segment)
                     ; timeout = None
                     }
              |> S.Engine.recv
                   ~action:
                     { time = 0.
                     ; action =
                         S.Action.AddLine
                           (S.LineSegmentRay.of_points
                              ~p1:{ x = 1100.; y = 100. }
                              ~p2:{ x = 700.; y = 700. }
                              ~kind:`Line)
                     ; timeout = None
                     }
              |> S.Engine.recv
                   ~action:
                     { time = 0.
                     ; action =
                         S.Action.AddLine
                           (S.LineSegmentRay.of_points
                              ~p1:{ x = 700.; y = 700. }
                              ~p2:{ x = 100.; y = 700. }
                              ~kind:`Ray)
                     ; timeout = None
                     }
              |> S.Engine.recv
                   ~action:
                     { time = 0.
                     ; action =
                         S.Action.AddLine
                           (S.LineSegmentRay.of_points
                              ~p1:{ x = 100.; y = 700. }
                              ~p2:{ x = 400.; y = 200. }
                              ~kind:`Segment)
                     ; timeout = None
                     })
            ~apply_action:(fun ~inject:_ ~schedule_event:_ model action ->
              match action with
              | `Replace _ as action -> S.Engine.update model ~action
              | `Action _ as action ->
                let _, diff = S.Engine.recv_with_diff model ~action in
                S.Engine.update model ~action:(`Diff diff))
        in
        SC.scene ~state ~dispatch
      ;;
    end
