open Core
open Bonsai_web
open Bonsai.Let_syntax
open Js_of_ocaml
module Svg = Virtual_dom_svg

module Make
    (C : Engine.Module_types.CONSTS with module N = Float)
    (S : module type of Engine.Scene.Make (C)) =
    struct
  let circle (id, figure) ~body_click =
    let x = S.Values.get_scalar_exn figure.S.Body.values ~var:`x0 in
    let y = S.Values.get_scalar_exn figure.values ~var:`y0 in
    let r = S.Values.get_scalar_exn figure.values ~var:`r in
    let on_click id (evt : Dom_html.mouseEvent Js.t) =
      let t1 = evt##.target |> Js.Opt.to_option in
      let t2 =
        t1
        |> Option.map ~f:Dom_svg.CoerceTo.element
        |> Option.bind ~f:Js.Opt.to_option
        |> Option.map ~f:Dom_svg.CoerceTo.circle
        |> Option.bind ~f:Js.Opt.to_option
      in
      match t1, t2 with
      | Some e1, Some e ->
        let dim = e1##getBoundingClientRect in
        let svg = e##.ownerSVGElement in
        let pt = svg##createSVGPoint in
        (Obj.magic pt)##.x := evt##.clientX;
        (Obj.magic pt)##.y := evt##.clientY;
        let cursorpt = pt##matrixTransform svg##getScreenCTM##inverse in
        Firebug.console##log evt;
        Firebug.console##log svg;
        Firebug.console##log pt;
        Firebug.console##log cursorpt;
        let x = dim##.left -. Int.to_float evt##.clientX in
        let y = dim##.top -. Int.to_float evt##.clientY in
        printf "x: %f, y: %f\n" x y;
        let _ = x, y in
        body_click id 0. 0. r
      | _ -> assert false
    in
    let open Vdom in
    Svg.Node.circle
      ~attr:
        (Attr.many
           [ Svg.Attr.cx x; Svg.Attr.cy y; Svg.Attr.r r; Attr.on_click (on_click id) ])
      []
  ;;

  let line S.LineSegmentRay.{ p1; p2; kind } =
    let open Float in
    let x1, y1, x2, y2 = p1.x, p1.y, p2.x, p2.y in
    let dx, dy = x2 - x1, y2 - y1 in
    let x1, y1, x2, y2 =
      match kind with
      | `Line -> x1 - (dx * 100.), y1 - (dy * 100.), x2 + (dx * 100.), y2 + (dy * 100.)
      | `Segment -> x1, y1, x2, y2
      | `Ray -> x1, y1, x2 + (dx * 100.), y2 + (dy * 100.)
    in
    let open Vdom in
    Svg.Node.line
      ~attr:
        (Attr.many
           [ Svg.Attr.x1 x1
           ; Svg.Attr.y1 y1
           ; Svg.Attr.x2 x2
           ; Svg.Attr.y2 y2
           ; Svg.Attr.stroke (`Name "black")
           ])
      []
  ;;

  let point S.Point.{ x; y } =
    let open Vdom in
    Svg.Node.circle ~attr:(Attr.many [ Svg.Attr.cx x; Svg.Attr.cy y; Svg.Attr.r 1. ]) []
  ;;

  let scene_frame
      ~scene
      ~(body_click : (S.Body.Id.t -> float -> float -> float -> unit Ui_effect.t) Value.t)
      ~time
      ~viewbox
    =
    let%arr (scene : S.Scene.t) = scene
    and body_click = body_click
    and time = time
    and min_x, min_y, width, height = viewbox in
    let t = time -. scene.time in
    let circles =
      scene.bodies
      |> S.Scene.Figures.calc ~t ~global_values:scene.global_values
      |> S.Scene.Figures.to_sequence
      |> Sequence.map ~f:(circle ~body_click)
    in
    let lines = scene.lines |> S.Lines.to_sequence |> Sequence.map ~f:line in
    let points = scene.points |> S.Points.to_sequence |> Sequence.map ~f:point in
    let all =
      circles |> Sequence.append lines |> Sequence.append points |> Sequence.to_list
    in
    let open Vdom in
    Svg.Node.svg
      ~attr:
        (Attr.many
           [ Attr.on_mousewheel (fun _event ->
                 (* Js_of_ocaml.Firebug.console##log event; *)
                 Effect.Ignore)
           ; Attr.on_mousedown (fun _event ->
                 (* Js_of_ocaml.Firebug.console##log event; *)
                 Effect.Ignore)
           ; Attr.on_mouseup (fun _event ->
                 (* Js_of_ocaml.Firebug.console##log event; *)
                 Effect.Ignore)
           ; Svg.Attr.viewbox ~min_x ~min_y ~width ~height
           ])
      all
  ;;

  let frame_time60 = 1. /. 60.
  let to_prec a = a *. 1000.
  let of_prec a = a /. 1000.

  let scene1 ~model ~time ~body_click ~viewbox =
    let%sub scene =
      let%arr model = model
      and time = time in
      model.S.Model.scenes |> S.Scenes.before ~time |> snd
    in
    scene_frame ~scene ~time ~body_click ~viewbox
  ;;

  let calc_new_time_every ~model ~time ~update_time ~is_pause ~speed =
    let%sub calc_new_time =
      let%arr is_pause = is_pause
      and speed = speed
      and model = model
      and time = time
      and update_time = update_time in
      let open Float in
      let p =
        match time + (frame_time60 * speed), is_pause with
        | _, true -> time
        | nt, _ when nt < 0. -> 0.
        | nt, false -> nt
      in
      let tm =
        match model.S.Model.timeout with
        | Some timeout -> min timeout p
        | None -> p
      in
      update_time tm
    in
    Bonsai.Clock.every [%here] (Time_ns.Span.of_sec frame_time60) calc_new_time
  ;;

  let scene ~(state : S.Model.t Value.t) ~dispatch =
    let%sub time, update_time = Bonsai.state [%here] (module Float) ~default_model:0. in
    let%sub is_pause, set_is_pause =
      Bonsai.state [%here] (module Bool) ~default_model:false
    in
    let%sub speed, set_speed = Bonsai.state [%here] (module Float) ~default_model:1. in
    let%sub timeoutd, set_timeoutd =
      Bonsai.state [%here] (module Float) ~default_model:10.
    in
    let%sub () = calc_new_time_every ~model:state ~time ~update_time ~is_pause ~speed in
    let%sub text_state, set_text_state =
      Bonsai.state [%here] (module String) ~default_model:""
    in
    let cl =
      dispatch
      >>| (fun a timeoutd time id x y r ->
            a
              (`Action
                S.Action.
                  { time
                  ; action =
                      S.Action.GiveVelocity
                        { id; v0 = Float.((x - r) / r * -200., (y - r) / r * -200.) }
                  ; timeout = Some Float.(time + timeoutd)
                  }))
      <*> timeoutd
      <*> time
    in
    let%sub frame =
      scene1
        ~model:state
        ~time
        ~body_click:cl
        ~viewbox:(Bonsai.Value.return (0., 0., 1000., 1000.))
    in
    let%arr time = time
    and frame = frame
    and dispatch = dispatch
    and is_pause = is_pause
    and set_is_pause = set_is_pause
    and speed = speed
    and set_speed = set_speed
    and set_time = update_time
    and text_state = text_state
    and set_text_state = set_text_state
    and state = state
    and timeoutd = timeoutd
    and set_timeoutd = set_timeoutd in
    Vdom.(
      ( Node.div
          [ Node.text (Float.to_string time)
          ; Node.br ()
          ; Node.button
              ~attr:
                (Attr.on_click (fun _ ->
                     `Action
                       { time
                       ; action =
                           S.Action.AddBody
                             { id = Some (S.Body.Id.next ())
                             ; x0 = 350.
                             ; y0 = 350.
                             ; r = 50.
                             ; mu = 2.
                             ; m = Float.(pi * 50. * 50.)
                             }
                       ; timeout = Some Float.(time + timeoutd)
                       }
                     |> dispatch))
              [ Node.text "add " ]
          ; Node.button
              ~attr:
                (Attr.on_click (fun _ ->
                     `Action
                       { time
                       ; action =
                           S.Action.AddBody
                             { id = Some (S.Body.Id.next ())
                             ; x0 = 800.
                             ; y0 = 500.
                             ; r = 75.
                             ; mu = 2.
                             ; m = Float.(pi * 75. * 75.)
                             }
                       ; timeout = Some Float.(time + timeoutd)
                       }
                     |> dispatch))
              [ Node.text "add " ]
          ; Node.button
              ~attr:
                (Attr.on_click (fun _ ->
                     `Action
                       { time
                       ; action =
                           S.Action.AddBody
                             { id = Some (S.Body.Id.next ())
                             ; x0 = 120.
                             ; y0 = 500.
                             ; r = 100.
                             ; mu = 2.
                             ; m = Float.(pi * 100. * 100.)
                             }
                       ; timeout = Some Float.(time + timeoutd)
                       }
                     |> dispatch))
              [ Node.text "add " ]
          ; Node.button
              ~attr:(Attr.on_click (fun _ -> set_is_pause (not is_pause)))
              [ Node.text (if is_pause then "unpause" else "pause") ]
          ; Node.br ()
          ; Node.input
              ~attr:
                (Attr.many
                   [ Attr.type_ "range"
                   ; Attr.min (to_prec (-25.))
                   ; Attr.max (to_prec 25.)
                   ; Attr.on_input (fun _ a ->
                         try a |> Float.of_string |> of_prec |> set_speed with
                         | _ -> Effect.Ignore)
                   ; Attr.value (Float.to_string speed)
                   ])
              []
          ; Node.input
              ~attr:
                (Attr.many
                   [ Attr.on_change (fun _ a ->
                         try a |> Float.of_string |> set_speed with
                         | _ -> Effect.Ignore)
                   ; Attr.value (Float.to_string speed)
                   ])
              []
          ; Node.br ()
          ; Node.input
              ~attr:
                (Attr.many
                   [ Attr.type_ "range"
                   ; Attr.min (to_prec 0.)
                   ; Attr.max (to_prec 100.)
                   ; Attr.on_change (fun _ a ->
                         try a |> Float.of_string |> of_prec |> set_time with
                         | _ -> Effect.Ignore)
                   ; Attr.value_prop (Float.to_string (to_prec time))
                   ])
              []
          ; Node.input
              ~attr:
                (Attr.many
                   [ Attr.on_change (fun _ a ->
                         try a |> Float.of_string |> set_time with
                         | _ -> Effect.Ignore)
                   ; Attr.value (Float.to_string time)
                   ])
              []
          ; Node.br ()
          ; Node.input
              ~attr:
                (Attr.many
                   [ Attr.type_ "range"
                   ; Attr.min (to_prec 0.)
                   ; Attr.max (to_prec 100.)
                   ; Attr.on_input (fun _ a ->
                         try a |> Float.of_string |> of_prec |> set_timeoutd with
                         | _ -> Effect.Ignore)
                   ; Attr.value (Float.to_string (to_prec timeoutd))
                   ])
              []
          ; Node.input
              ~attr:
                (Attr.many
                   [ Attr.on_change (fun _ a ->
                         try a |> Float.of_string |> set_timeoutd with
                         | _ -> Effect.Ignore)
                   ; Attr.value (Float.to_string timeoutd)
                   ])
              []
          ; Node.br ()
          ; Node.button
              ~attr:
                (Attr.on_click (fun _ ->
                     `Action
                       { time
                       ; action = S.Action.Empty
                       ; timeout = Some Float.(time + timeoutd)
                       }
                     |> dispatch))
              [ Node.text "timeout" ]
          ; Node.button
              ~attr:
                (Attr.on_click (fun _ ->
                     `Action { time; action = S.Action.Empty; timeout = None } |> dispatch))
              [ Node.text "no timeout" ]
          ; Node.br ()
          ]
      , Node.div
          [ frame
          ; Node.textarea
              ~attr:
                (Attr.many
                   [ Attr.value_prop text_state; Attr.on_input (fun _ -> set_text_state) ])
              []
          ; Node.button
              ~attr:
                (Attr.many
                   [ Attr.on_click (fun _ ->
                         [%sexp (state : S.Model.t)]
                         |> Sexp.to_string_hum
                         |> set_text_state)
                   ])
              [ Node.text "to text" ]
          ] ))
  ;;
end
