open Core
open Bonsai_web
open Bonsai.Let_syntax
open Js_of_ocaml
module Svg = Virtual_dom_svg
module S = Chapgame.Scene.Make (Float)

let eps = 1e-7

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
  Svg.Node.svg
    (scene.figures
    |> S.Scene.Figures.calc ~t ~global_values:scene.global_values
    |> S.Scene.Figures.to_sequence
    |> Sequence.map ~f:(fun (id, figure) ->
           circle
             ~c:
               { id
               ; x = S.Values.get_scalar_exn figure.values ~var:`x0
               ; y = S.Values.get_scalar_exn figure.values ~var:`y0
               ; r = S.Values.get_scalar_exn figure.values ~var:`r
               }
             ~on_click:(fun id event ->
               Firebug.console##log event;
               on_click id))
    |> Sequence.to_list)
;;

let frame_time60 = 1. /. 60.

let scene =
  let%sub state, dispatch =
    Bonsai.state_machine0
      [%here]
      (module S.Model)
      (module S.Action)
      ~default_model:(S.Model.empty ~g:10.)
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model action ->
        let ret = S.Engine.recv model ~action ~eps in
        print_s [%sexp (ret : S.Model.t)];
        ret)
  in
  let%sub time, update_time = Bonsai.state [%here] (module Float) ~default_model:0. in
  let%sub () =
    Bonsai.Clock.every
      [%here]
      (Time_ns.Span.of_sec frame_time60)
      (update_time <*> (time >>| fun t -> t +. frame_time60))
  in
  let%sub last_scene =
    let%arr state = state
    and time = time in
    state |> S.Model.before ~time |> S.Model.last_exn
  in
  let cl =
    dispatch
    >>| (fun a time id ->
          a
            { time
            ; action =
                S.Action.GiveVelocity
                  { id
                  ; v0 =
                      (match id with
                      | 0 -> 25., 25.
                      | 1 -> -25., -25.
                      | _ -> assert false)
                  }
            })
    <*> time
  in
  let%sub frame = scene_frame ~scene:last_scene ~time ~on_click:cl in
  let%arr time = time
  and frame = frame
  and dispatch = dispatch in
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
                       { id = 0; x0 = 25.; y0 = 25.; r = 10.; mu = 1.; m = 1. }
                 }
                 |> dispatch))
          [ Node.text "add 0" ]
      ; Node.button
          ~attr:
            (Attr.on_click (fun _ ->
                 { time
                 ; action =
                     S.Action.AddBody
                       { id = 1; x0 = 100.; y0 = 100.; r = 15.; mu = 1.; m = 1. }
                 }
                 |> dispatch))
          [ Node.text "add 1" ]
      ; Node.br ()
      ; frame
      ])
;;
