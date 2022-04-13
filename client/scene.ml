open Core
open Bonsai_web
open Bonsai.Let_syntax
open Js_of_ocaml
module Svg = Virtual_dom_svg
module S = Chapgame.Scene.Make (Float)

let eps = 1e-6

type circle =
  { id : S.Figure2.Id.t
  ; x : float
  ; y : float
  ; r : float
  }

let circle ~c ~on_click =
  let { id; x; y; r } = c in
  Vdom.(
    Svg.Node.circle
      ~attr:
        (Attr.many
           [ Svg.Attr.cx x; Svg.Attr.cy y; Svg.Attr.r r; Attr.on_click (on_click id) ])
      [])
;;

let scene_frame ~scene ~on_click ~time =
  let%arr (scene : S.Scene.t) = scene
  and on_click = on_click
  and time = time in
  let t = time -. scene.time in
  Vdom.(
    Svg.Node.svg
      ~attr:(Attr.many [ Svg.Attr.width 1280.; Svg.Attr.height 720. ])
      (scene.bodies
      |> S.Scene.Figures.calc ~t ~global_values:scene.global_values ~eps
      |> S.Scene.Figures.to_sequence
      |> Sequence.map ~f:(fun (id, figure) ->
             circle
               ~c:
                 { id
                 ; x = S.Values.get_scalar_exn figure.values ~var:`x0
                 ; y = S.Values.get_scalar_exn figure.values ~var:`y0
                 ; r = S.Values.get_scalar_exn figure.values ~var:`r
                 }
               ~on_click:(fun id evt ->
                 let e = (evt##.target |> Js.Opt.get) (fun _ -> assert false) in
                 let dim = e##getBoundingClientRect in
                 let r = S.Values.get_scalar_exn figure.values ~var:`r in
                 let x = Float.(of_int evt##.clientX - dim##.left) in
                 let y = Float.(of_int evt##.clientY - dim##.top) in
                 on_click id x y r))
      |> Sequence.to_list))
;;

let frame_time60 = 1. /. 60.

let scene =
  let%sub state, dispatch =
    Bonsai.state_machine0
      [%here]
      (module S.Model)
      (module S.Action)
      ~default_model:
        (S.Model.empty ~g:10.
        |> S.Engine.recv
             ~action:
               { time = 0.
               ; action =
                   S.Action.AddBody
                     { id = S.Figure2.Id.next ()
                     ; x0 = 250.
                     ; y0 = 250.
                     ; r = 50.
                     ; mu = 2.
                     ; m = 1.
                     }
               }
             ~eps
        |> S.Engine.recv
             ~action:
               { time = 0.
               ; action =
                   S.Action.AddBody
                     { id = S.Figure2.Id.next ()
                     ; x0 = 500.
                     ; y0 = 200.
                     ; r = 75.
                     ; mu = 2.
                     ; m = 2.
                     }
               }
             ~eps
        |> S.Engine.recv
             ~action:
               { time = 0.
               ; action =
                   S.Action.AddBody
                     { id = S.Figure2.Id.next ()
                     ; x0 = 500.
                     ; y0 = 500.
                     ; r = 100.
                     ; mu = 2.
                     ; m = 3.
                     }
               }
             ~eps)
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model action ->
        let ret = S.Engine.recv model ~action ~eps in
        print_s [%sexp (ret : S.Model.t)];
        ret)
  in
  let%sub time, update_time = Bonsai.state [%here] (module Float) ~default_model:0. in
  let%sub is_pause, set_is_pause =
    Bonsai.state [%here] (module Bool) ~default_model:false
  in
  let%sub speed, set_speed = Bonsai.state [%here] (module Float) ~default_model:1. in
  let%sub a =
    let%arr is_pause = is_pause
    and speed = speed in
    fun t ->
      let open Float in
      match t + (frame_time60 * speed), is_pause with
      | _, true -> t
      | nt, _ when nt < 0. -> 0.
      | nt, false -> nt
  in
  let%sub () =
    Bonsai.Clock.every
      [%here]
      (Time_ns.Span.of_sec frame_time60)
      (update_time <*> (a <*> time))
  in
  let%sub last_scene =
    let%arr state = state
    and time = time in
    state |> S.Model.before ~time |> snd
  in
  let cl =
    dispatch
    >>| (fun a time id x y r ->
          a
            { time
            ; action =
                S.Action.GiveVelocity
                  { id; v0 = Float.((x - r) / r * -200., (y - r) / r * -200.) }
            })
    <*> time
  in
  let%sub frame = scene_frame ~scene:last_scene ~time ~on_click:cl in
  let%arr time = time
  and frame = frame
  and dispatch = dispatch
  and is_pause = is_pause
  and set_is_pause = set_is_pause
  and speed = speed
  and set_speed = set_speed
  and set_time = update_time in
  Vdom.(
    Node.div
      [ Node.text (Float.to_string time)
      ; Node.br ()
      ; Node.button
          ~attr:
            (Attr.on_click (fun _ ->
                 { time
                 ; action =
                     S.Action.AddBody
                       { id = S.Figure2.Id.next ()
                       ; x0 = 350.
                       ; y0 = 350.
                       ; r = 50.
                       ; mu = 2.
                       ; m = 1.
                       }
                 }
                 |> dispatch))
          [ Node.text "add " ]
      ; Node.button
          ~attr:
            (Attr.on_click (fun _ ->
                 { time
                 ; action =
                     S.Action.AddBody
                       { id = S.Figure2.Id.next ()
                       ; x0 = 700.
                       ; y0 = 500.
                       ; r = 75.
                       ; mu = 2.
                       ; m = 2.
                       }
                 }
                 |> dispatch))
          [ Node.text "add " ]
      ; Node.button
          ~attr:
            (Attr.on_click (fun _ ->
                 { time
                 ; action =
                     S.Action.AddBody
                       { id = S.Figure2.Id.next ()
                       ; x0 = 120.
                       ; y0 = 500.
                       ; r = 100.
                       ; mu = 2.
                       ; m = 3.
                       }
                 }
                 |> dispatch))
          [ Node.text "add " ]
      ; Node.button
          ~attr:(Attr.on_click (fun _ -> set_is_pause (not is_pause)))
          [ Node.text (if is_pause then "unpause" else "pause") ]
      ; Node.input
          ~attr:
            (Attr.many
               [ Attr.type_ "range"
               ; Attr.min (-25.)
               ; Attr.max 25.
               ; Attr.on_input (fun _ a ->
                     try a |> Float.of_string |> set_speed with
                     | _ -> Effect.Ignore)
               ; Attr.value (Float.to_string speed)
               ])
          []
      ; Node.input
          ~attr:
            (Attr.many
               [ Attr.type_ "range"
               ; Attr.min 0.
               ; Attr.max 100.
               ; Attr.on_input (fun _ a ->
                     try a |> Float.of_string |> set_time with
                     | _ -> Effect.Ignore)
               ; Attr.value_prop (Float.to_string time)
               ])
          []
      ; Node.br ()
      ; frame
      ])
;;
