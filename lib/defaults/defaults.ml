open Core
module C = (val Engine.Utils.make_consts ~eps:1e-6)
module S = Engine.Scene.Make (C)
module Request = Protocol.Request.Make (C) (S)

module Response =
  Protocol.Response.Make (C) (S)
    (struct
      include Unit
    end)

module Replays = struct
  let actions =
    [ S.Action.AddBody
        { id = Some (S.Body.Id.next ())
        ; x0 = 425.
        ; y0 = 275.
        ; r = 2.
        ; mu = 50.
        ; m = Float.(pi * 2. * 2.)
        }
    ; S.Action.AddBody
        { id = Some (S.Body.Id.next ())
        ; x0 = 450.
        ; y0 = 250.
        ; r = 10.
        ; mu = 10.
        ; m = Float.(pi * 10. * 10.)
        }
    ; S.Action.AddBody
        { id = Some (S.Body.Id.next ())
        ; x0 = 600.
        ; y0 = 600.
        ; r = 50.
        ; mu = 20.
        ; m = Float.(pi * 50. * 50.)
        }
    ; S.Action.AddBody
        { id = Some (S.Body.Id.next ())
        ; x0 = 500.
        ; y0 = 500.
        ; r = 60.
        ; mu = 30.
        ; m = Float.(pi * 60. * 60.)
        }
    ; S.Action.AddPoint { x = 400.; y = 200. }
    ; S.Action.AddPoint { x = 1100.; y = 100. }
    ; S.Action.AddPoint { x = 100.; y = 700. }
    ; S.Action.AddPoint { x = 700.; y = 700. }
    ; S.Action.AddPoint { x = 650.; y = 325. }
    ; S.Action.AddPoint { x = 600.; y = 400. }
    ; S.Action.AddPoint { x = 700.; y = 450. }
    ; S.Action.AddLine
        (S.LineSegmentRay.of_points
           ~p1:{ x = 650.; y = 325. }
           ~p2:{ x = 600.; y = 400. }
           ~kind:`Segment)
    ; S.Action.AddLine
        (S.LineSegmentRay.of_points
           ~p1:{ x = 600.; y = 400. }
           ~p2:{ x = 700.; y = 450. }
           ~kind:`Segment)
    ; S.Action.AddLine
        (S.LineSegmentRay.of_points
           ~p1:{ x = 400.; y = 200. }
           ~p2:{ x = 1100.; y = 100. }
           ~kind:`Segment)
    ; S.Action.AddLine
        (S.LineSegmentRay.of_points
           ~p1:{ x = 1100.; y = 100. }
           ~p2:{ x = 700.; y = 700. }
           ~kind:`Line)
    ; S.Action.AddLine
        (S.LineSegmentRay.of_points
           ~p1:{ x = 700.; y = 700. }
           ~p2:{ x = 100.; y = 700. }
           ~kind:`Ray)
    ; S.Action.AddLine
        (S.LineSegmentRay.of_points
           ~p1:{ x = 100.; y = 700. }
           ~p2:{ x = 400.; y = 200. }
           ~kind:`Segment)
    ]
  ;;

  let start =
    lazy
      (actions
      |> List.fold ~init:(S.Model.init ~g:1.) ~f:(fun acc action ->
             S.Engine.recv
               ~action:
                 { time = 0.
                 ; action
                 ; until = { timespan = None; quantity = None; stability = true }
                 }
               acc))
  ;;

  let replay = function
    | "start" -> Lazy.force start
    | _ -> assert false
  ;;
end
