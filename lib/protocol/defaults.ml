open Core
module Response = Response.Make (Unit)
module S = Engine

module Replays = struct
  let actions =
    [ S.Action.AddBody
        { id = Some (S.Body.Id.next ())
        ; x0 = 425.
        ; y0 = 275.
        ; r = 2.
        ; mu = 50.
        ; m = Float.(3. * 2. * 2.)
        }
    ; S.Action.AddBody
        { id = Some (S.Body.Id.next ())
        ; x0 = 450.
        ; y0 = 250.
        ; r = 10.
        ; mu = 10.
        ; m = Float.(3. * 10. * 10.)
        }
    ; S.Action.AddBody
        { id = Some (S.Body.Id.next ())
        ; x0 = 600.
        ; y0 = 600.
        ; r = 50.
        ; mu = 20.
        ; m = Float.(3. * 50. * 50.)
        }
    ; S.Action.AddBody
        { id = Some (S.Body.Id.next ())
        ; x0 = 500.
        ; y0 = 500.
        ; r = 60.
        ; mu = 30.
        ; m = Float.(3. * 60. * 60.)
        }
    ; S.Action.AddPoint { x = 400.; y = 200. }
    ; S.Action.AddPoint { x = 1100.; y = 100. }
    ; S.Action.AddPoint { x = 100.; y = 700. }
    ; S.Action.AddPoint { x = 700.; y = 700. }
    ; S.Action.AddPoint { x = 650.; y = 325. }
    ; S.Action.AddPoint { x = 600.; y = 400. }
    ; S.Action.AddPoint { x = 700.; y = 450. }
    ; S.Action.AddLine
        (S.Line.of_points
           ~p1:{ x = 650.; y = 325. }
           ~p2:{ x = 600.; y = 400. }
           ~kind:`Segment)
    ; S.Action.AddLine
        (S.Line.of_points
           ~p1:{ x = 600.; y = 400. }
           ~p2:{ x = 700.; y = 450. }
           ~kind:`Segment)
    ; S.Action.AddLine
        (S.Line.of_points
           ~p1:{ x = 400.; y = 200. }
           ~p2:{ x = 1100.; y = 100. }
           ~kind:`Segment)
    ; S.Action.AddLine
        (S.Line.of_points
           ~p1:{ x = 1100.; y = 100. }
           ~p2:{ x = 700.; y = 700. }
           ~kind:`Line)
    ; S.Action.AddLine
        (S.Line.of_points
           ~p1:{ x = 700.; y = 700. }
           ~p2:{ x = 100.; y = 700. }
           ~kind:`Ray)
    ; S.Action.AddLine
        (S.Line.of_points
           ~p1:{ x = 100.; y = 700. }
           ~p2:{ x = 400.; y = 200. }
           ~kind:`Segment)
    ]
  ;;

  let start =
    lazy
      (actions
      |> List.fold ~init:(S.Model.init ~g:1.) ~f:(fun acc action ->
             S.update
               ~action:
                 (`Action
                   S.Action.
                     { time = 0.; action; until = { timespan = None; quantity = None } })
               acc))
  ;;

  let square S.Point.{ x = x1; y = y1 } S.Point.{ x = x2; y = y2 } =
    [ S.Action.AddLine
        (S.Line.of_points ~p1:{ x = x1; y = y1 } ~p2:{ x = x1; y = y2 } ~kind:`Segment)
    ; S.Action.AddLine
        (S.Line.of_points ~p1:{ x = x1; y = y1 } ~p2:{ x = x2; y = y1 } ~kind:`Segment)
    ; S.Action.AddLine
        (S.Line.of_points ~p1:{ x = x2; y = y2 } ~p2:{ x = x1; y = y2 } ~kind:`Segment)
    ; S.Action.AddLine
        (S.Line.of_points ~p1:{ x = x2; y = y2 } ~p2:{ x = x2; y = y1 } ~kind:`Segment)
    ]
  ;;

  let unfold actions =
    lazy
      (actions
      |> List.fold ~init:(S.Model.init ~g:1.) ~f:(fun acc action ->
             S.update
               ~action:
                 (`Action
                   S.Action.
                     { time = 0.; action; until = { timespan = None; quantity = Some 1 } })
               acc))
  ;;

  let brownian_1 =
    let left, right, top, bottom = 200., 600., 200., 600. in
    let square = square S.Point.{ x = left; y = top } S.Point.{ x = right; y = bottom } in
    let actions =
      Sequence.range 0 30
      |> Sequence.map ~f:(fun _ ->
             let open Float in
             let r = 12. in
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
    square @ actions |> unfold
  ;;

  let replay = function
    | "start" -> Lazy.force start
    | "brownian_1" -> Lazy.force brownian_1
    | _ -> assert false
  ;;
end
