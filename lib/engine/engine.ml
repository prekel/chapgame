open Core
open Open
module Var = Var
module Scope = Scope
module Values = Values
module Line = Line
module Point = Point
module Body = Body
module Action = Action
module Model = Model
module Scene = Scene
module Bodies = Bodies
module Lines = Lines
module Points = Points
module Scenes = Scenes

let forward_seq ?time (scene : Scene.t) =
  Sequence.unfold ~init:(Some scene) ~f:(function
      | None -> None
      | Some scene ->
        let mt =
          [ Bodies.to_sequence scene.bodies
            |> Collision_detection.WithBody.first_collision
                 ~global:scene.global_values
                 ~r:(Formula.of_alist_exn Rule.Exprs.[ 0, r ])
            |> Option.map ~f:(fun ((t, _, _) as a) -> t, `WithBody a)
          ; Collision_detection.WithPoint.first_collision
              ~global:scene.global_values
              ~points:(Points.to_sequence scene.points)
              ~r:(Formula.of_alist_exn Rule.Exprs.[ 0, r ])
              (Bodies.to_sequence scene.bodies)
            |> Option.map ~f:(fun ((t, _, _) as a) -> t, `WithPoint a)
          ; Collision_detection.WithLine.first_collision
              ~global:scene.global_values
              ~lines:(Lines.to_sequence scene.lines)
              ~r:(Formula.of_alist_exn Rule.Exprs.[ 0, r ])
              (Bodies.to_sequence scene.bodies)
            |> Option.map ~f:(fun ((t, _, _) as a) -> t, `WithLine a)
          ]
          |> List.filter_opt
          |> List.min_elt ~compare:(fun (t1, _) (t2, _) -> Float.compare t1 t2)
          |> Option.map ~f:snd
        in
        let time_lt_scene_time =
          let open Option.Let_syntax in
          let%bind s =
            match%map mt with
            | `WithBody (t, id1, id2) ->
              let q = Bodies.calc scene.bodies ~t ~global_values:scene.global_values in
              let body1 = Bodies.get_by_id q ~id:id1 in
              let body2 = Bodies.get_by_id q ~id:id2 in
              let v1n, v2n = Collision_handle.calculate_new_v body1.values body2.values in
              Scene.update
                scene
                ~bodies:
                  (q
                  |> Bodies.update_by_id
                       ~id:id1
                       ~body:(Body.update_v0 body1 ~v:v1n ~rules:Rule.rules1)
                  |> Bodies.update_by_id
                       ~id:id2
                       ~body:(Body.update_v0 body2 ~v:v2n ~rules:Rule.rules1))
                ~cause:[ `Collision (Collision { id1; id2 }) ]
                ~time:Float.(scene.time + t)
                ~points:scene.points
                ~lines:scene.lines
            | `WithPoint (t, id, point) ->
              let q = Bodies.calc scene.bodies ~t ~global_values:scene.global_values in
              let body = Bodies.get_by_id q ~id in
              let v' = Collision_handle.calculate_new_v_with_point ~body ~point in
              let new_time = Float.(scene.time + t) in
              Scene.update
                scene
                ~bodies:
                  (Bodies.update_by_id
                     q
                     ~id
                     ~body:(Body.update_v0 body ~v:v' ~rules:Rule.rules1))
                ~cause:[ `Collision (CollisionWithPoint { id; point }) ]
                ~time:new_time
                ~points:scene.points
                ~lines:scene.lines
            | `WithLine (t, id, line) ->
              let q = Bodies.calc scene.bodies ~t ~global_values:scene.global_values in
              let body = Bodies.get_by_id q ~id in
              let v' = Collision_handle.calculate_new_v_with_line ~body ~line in
              let new_time = Float.(scene.time + t) in
              Scene.update
                scene
                ~bodies:
                  (Bodies.update_by_id
                     q
                     ~id
                     ~body:(Body.update_v0 body ~v:v' ~rules:Rule.rules1))
                ~cause:[ `Collision (CollisionWithLine { id; line }) ]
                ~time:new_time
                ~points:scene.points
                ~lines:scene.lines
          in
          match time with
          | Some time when Float.(s.Scene.time > time) -> Some (s, Some time)
          | Some _ | None -> Some (s, None)
        in
        (match time_lt_scene_time, time with
        | Some (s, None), _ -> Some (s, Some s)
        | Some (_, Some time), _ | None, Some time ->
          Some
            ( Scene.update
                scene
                ~bodies:
                  (Bodies.calc
                     scene.bodies
                     ~t:Float.(time - scene.time)
                     ~global_values:scene.global_values)
                ~cause:[]
                ~time
                ~points:scene.points
                ~lines:scene.lines
            , None )
        | None, None -> None))
;;

let forward ?time (scene : Scene.t) ~timeout ~quantity =
  let filter_timeout =
    match timeout with
    | Some timeout -> Sequence.take_while ~f:Float.(fun s -> s.Scene.time <= timeout)
    | None -> Fn.id
  in
  let filter_quantity =
    match quantity with
    | Some quantity -> fun s -> Sequence.take s quantity
    | None -> Fn.id
  in
  let reversed =
    forward_seq ?time scene |> filter_timeout |> filter_quantity |> Sequence.to_list_rev
  in
  let timeout =
    match timeout, quantity, List.hd reversed with
    | _, Some _, Some last -> Some last.time
    | Some timeout, _, _ -> Some timeout
    | _ -> None
  in
  timeout, List.rev reversed
;;

let apply_action s ~time = function
  | Action.AddBody { id; x0; y0; r; mu; m } ->
    let id =
      match id with
      | Some id -> id
      | None -> Body.Id.next ()
    in
    Scene.update s ~bodies:(Scene.add_body s.Scene.bodies ~id ~x0 ~y0 ~r ~mu ~m) ~time
  | AddBodyOfValues (id, values) ->
    let values = Values.of_alist values in
    Scene.update
      s
      ~bodies:
        (Scene.add_body_values
           s.Scene.bodies
           ~id:
             (match id with
             | Some id -> id
             | None -> Body.Id.next ())
           ~values
           ~rules:(Rule.of_values values))
      ~time
  | AddPoint point -> Scene.update s ~points:(Points.add s.points ~el:point) ~time
  | AddLine line -> Scene.update s ~lines:(Lines.add s.lines ~el:line) ~time
  | AddLineWithPoints line ->
    let lines = Lines.add s.lines ~el:line in
    let points =
      match line.kind with
      | `Segment -> s.points |> Points.add ~el:line.p1 |> Points.add ~el:line.p2
      | `Ray -> s.points |> Points.add ~el:line.p1
      | `Line -> s.points
    in
    Scene.update s ~lines ~time ~points
  | GiveVelocity { id; v0 } ->
    let body = Bodies.get_by_id s.bodies ~id in
    let body = Body.update_v0 body ~v:v0 ~rules:Rule.rules1 in
    Scene.update s ~bodies:(Bodies.update_by_id s.bodies ~id:body.id ~body) ~time
  | RemoveBody id -> Scene.update s ~bodies:(Bodies.remove s.bodies id) ~time
  | RemoveLine line -> Scene.update s ~lines:(Lines.remove s.lines ~el:line) ~time
  | RemovePoint point -> Scene.update s ~points:(Points.remove s.points ~el:point) ~time
  | UpdateBody (id, updated) ->
    let body = Bodies.get_by_id s.bodies ~id in
    let values =
      List.fold updated ~init:body.values ~f:(fun acc (var, value) ->
          Values.update_scalar acc ~var ~value)
    in
    let rules = Rule.of_values values in
    let body = Body.{ body with values; rules } in
    Scene.update s ~bodies:(Bodies.update_by_id s.bodies ~id:body.id ~body) ~time
  | UpdateLine (old, new_) ->
    let lines = s.lines |> Lines.remove ~el:old |> Lines.add ~el:new_ in
    Scene.update s ~lines ~time
  | UpdatePoint (old, new_) ->
    let points = s.points |> Points.remove ~el:old |> Points.add ~el:new_ in
    Scene.update s ~points ~time
  | UpdateGlobal (var, value) ->
    Scene.{ s with global_values = Values.update_scalar s.global_values ~var ~value }
;;

let recv Model.{ scenes; _ } ~action:Action.({ time; action; until } as ac) =
  let timeout = Option.map until.timespan ~f:(fun t -> Float.(time + t)) in
  let quantity = until.quantity in
  let before, s = Scenes.before scenes ~time in
  let _new_timeout, scenes = forward s ~time ~timeout:(Some time) ~quantity:None in
  let scenes = Scenes.merge_with_list before scenes in
  let before, s = Scenes.before scenes ~time in
  let r = { (apply_action ~time s action) with cause = [ `Action ac ] } in
  let m = before |> Model.of_scenes ~time:r.time ~scene:r ~timeout in
  let new_timeout1, f = forward (Scenes.last_exn m.scenes) ~timeout ~quantity in
  Model.{ scenes = Scenes.merge_with_list m.scenes f; timeout = new_timeout1 }
;;

let prolong (model : Model.t) ~(until : Action.until) =
  let timeout =
    let%bind.Option new_timespan = until.timespan in
    let%map.Option old_timeout = model.timeout in
    Float.(old_timeout + new_timespan)
  in
  let quantity = until.quantity in
  let new_timeout, after = forward (Scenes.last_exn model.scenes) ~timeout ~quantity in
  Model.{ scenes = Scenes.merge_with_list model.scenes after; timeout = new_timeout }
;;

let update model ~action =
  match action with
  | `Action a -> recv model ~action:a
  | `Replace m -> m
  | `Diff diff -> Model.Diff.apply_diff model ~diff
  | `Prolong until -> prolong model ~until
;;

let recv_with_diff model ~action =
  match action with
  | `Action a ->
    let updated = recv model ~action:a in
    updated, Model.Diff.diff ~old:model updated
;;
