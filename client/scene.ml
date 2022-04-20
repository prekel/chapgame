open Core
open Bonsai_web
open Bonsai.Let_syntax
open Js_of_ocaml
module Svg = Virtual_dom_svg
module P = Chapgame.Protocol.Make (Float) ((val Chapgame.Utils.make_consts ~eps:1e-6))
module S = P.SC

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
               ~on_click:(fun id evt ->
                 let e = (evt##.target |> Js.Opt.get) (fun _ -> assert false) in
                 let dim = e##getBoundingClientRect in
                 let r = S.Values.get_scalar_exn figure.values ~var:`r in
                 let x = Float.(of_int evt##.clientX - dim##.left) in
                 let y = Float.(of_int evt##.clientY - dim##.top) in
                 on_click id x y r))
      |> Sequence.append
           (scene.points
           |> S.Points.to_sequence
           |> Sequence.map ~f:(fun S.Point.{ x; y } ->
                  Svg.Node.circle
                    ~attr:(Attr.many [ Svg.Attr.cx x; Svg.Attr.cy y; Svg.Attr.r 1. ])
                    []))
      |> Sequence.append
           (scene.lines
           |> S.Lines.to_sequence
           |> Sequence.map ~f:(fun { p1; p2; kind } ->
                  let open Float in
                  let x1, y1, x2, y2 = p1.x, p1.y, p2.x, p2.y in
                  let dx, dy = x2 - x1, y2 - y1 in
                  let x1, y1, x2, y2 =
                    match kind with
                    | `Line ->
                      ( x1 - (dx * 100.)
                      , y1 - (dy * 100.)
                      , x2 + (dx * 100.)
                      , y2 + (dy * 100.) )
                    | `Segment -> x1, y1, x2, y2
                    | `Ray -> x1, y1, x2 + (dx * 100.), y2 + (dy * 100.)
                  in
                  Svg.Node.line
                    ~attr:
                      (Attr.many
                         [ Svg.Attr.x1 x1
                         ; Svg.Attr.y1 y1
                         ; Svg.Attr.x2 x2
                         ; Svg.Attr.y2 y2
                         ; Svg.Attr.stroke (`Name "black")
                         ])
                    []))
      |> Sequence.to_list))
;;

let frame_time60 = 1. /. 60.
let to_prec a = a *. 1000.
let of_prec a = a /. 1000.

let scene ~(state : S.Model.t Value.t) ~dispatch =
  let%sub time, update_time = Bonsai.state [%here] (module Float) ~default_model:0. in
  let%sub is_pause, set_is_pause =
    Bonsai.state [%here] (module Bool) ~default_model:false
  in
  let%sub speed, set_speed = Bonsai.state [%here] (module Float) ~default_model:1. in
  let%sub timeoutd, set_timeoutd =
    Bonsai.state [%here] (module Float) ~default_model:10.
  in
  let%sub a =
    let%arr is_pause = is_pause
    and speed = speed
    and state = state in
    fun t ->
      let open Float in
      let p =
        match t + (frame_time60 * speed), is_pause with
        | _, true -> t
        | nt, _ when nt < 0. -> 0.
        | nt, false -> nt
      in
      match state.timeout with
      | Some timeout -> min timeout p
      | None -> p
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
    state.scenes |> S.Model.Scenes.before ~time |> snd
  in
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
  let%sub frame = scene_frame ~scene:last_scene ~time ~on_click:cl in
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
    Node.div
      [ Node.text (Float.to_string time)
      ; Node.br ()
      ; Node.button
          ~attr:
            (Attr.on_click (fun _ ->
                 `Action
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
                         { id = S.Figure2.Id.next ()
                         ; x0 = 800.
                         ; y0 = 500.
                         ; r = 75.
                         ; mu = 2.
                         ; m = 2.
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
                         { id = S.Figure2.Id.next ()
                         ; x0 = 120.
                         ; y0 = 500.
                         ; r = 100.
                         ; mu = 2.
                         ; m = 3.
                         }
                   ; timeout = Some Float.(time + timeoutd)
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
      ; frame
      ; Node.textarea
          ~attr:
            (Attr.many
               [ Attr.value_prop text_state; Attr.on_input (fun _ -> set_text_state) ])
          []
      ; Node.button
          ~attr:
            (Attr.many
               [ Attr.on_click (fun _ ->
                     [%sexp (state : S.Model.t)] |> Sexp.to_string_hum |> set_text_state)
               ])
          [ Node.text "to text" ]
      ; Node.button
          ~attr:
            (Attr.many
               [ Attr.on_click (fun _ ->
                     text_state
                     |> Sexp.of_string
                     |> S.Model.t_of_sexp
                     |> (fun a -> `Replace a)
                     |> dispatch)
               ])
          [ Node.text "of text" ]
      ])
;;

(* let online ~init = let%sub state, dispatch = Bonsai_extra.state_machine0_dynamic_model
   [%here] (module S.Model) (module struct type t = [ `Diff of S.Model.Diff.diff ]
   [@@deriving sexp, equal] end) ~model:(`Given init) ~apply_action:(fun ~inject:_
   ~schedule_event:_ model action -> match action with | `Diff _ as action ->
   S.Engine.update model ~action) in let%sub w, s = Bonsai.state_machine1 [%here] (module
   struct type t = Websocket.t option [@@deriving sexp, equal] end) (module struct type t
   = [ `Init | `Send of P.Request.t ] [@@deriving sexp, equal] end) ~default_model:None
   dispatch ~apply_action:(fun ~inject:_ ~schedule_event dispatch model action -> match
   model, action with | None, `Init -> Some (Websocket.connect (Uri.of_string ("ws://" ^
   "localhost:8080" ^ "/room/0/ws")) ~on_message:(fun msg -> let (Diff diff) = msg |>
   Sexp.of_string |> [%of_sexp: P.Response.t] in `Diff diff |> dispatch |>
   schedule_event)) | Some w, `Send s -> Websocket.send w (Sexp.to_string_hum [%sexp (s :
   P.Request.t)]); model | _ -> raise_s [%message (model : Websocket.t option) (action : [
   `Init | `Send of P.Request.t ])]) in let%sub dispatch = let%arr s = s and w = w in
   function | `Action a -> begin match w with | None -> let%bind.Effect _ = `Init |> s in
   `Send (P.Request.Action a) |> s | Some _ -> `Send (P.Request.Action a) |> s end |
   `Replace _ -> assert false in scene ~state ~dispatch ;;

   let onlineinit ~room_id = let%sub init, set_init = Bonsai.state_machine1 [%here]
   (module struct type t = S.Model.t option [@@deriving sexp, equal] end) (module struct
   type t = [ `Init ] [@@deriving sexp, equal] end) room_id ~default_model:None
   ~apply_action:(fun ~inject:_ ~schedule_event room_id model action -> match model,
   action with | None, `Init -> (* Effect_lwt.of_lwt_fun (fun () ->
   Cohttp_lwt_jsoo.Client.get (Uri.of_string ("http://" ^ "localhost:8080" ^ "/room/" ^
   Int.to_string room_id))) () |> schedule_event; *) assert false | _ -> assert false) in
   match%sub init with | Some init -> online ~init | None -> let%arr set_init = set_init
   in Vdom.(Node.button ~attr:Attr.(many [ on_click (fun _ -> set_init `Init) ]) []) ;; *)
(* Bonsai. let%sub q = let mp = Bonsai.Value.map init ~f:(fun a -> Option.value_exn a) in
   online ~init:mp in Bonsai.Computation.read q *)

(* let%sub online = match init with | Some init -> let%sub o = online
   ~init:(Bonsai.Value.return init) in let%arr o = o in o | None -> Vdom.(Node.button
   ~attr:Attr.(many [ on_click (fun _ -> set_init `Init) ]) []) in let%arr online = online
   in online *)

let connect ~var ~room_id =
  Websocket.connect
    (Uri.of_string
       ("ws://" ^ "localhost:8080" ^ "/room/" ^ Int.to_string room_id ^ "/ws"))
    ~on_message:(fun msg ->
      let (Diff diff) = msg |> Sexp.of_string |> [%of_sexp: P.Response.t] in
      Bonsai.Var.update var ~f:(fun prev -> S.Engine.update prev ~action:(`Diff diff)))
;;

(* let online ~state_var ~room_id = let%sub ws = Bonsai_extra.thunk (fun () -> connect
   ~var:state_var ~room_id) in let%sub dispatch = let%arr ws = ws in Effect.of_sync_fun
   (function | `Action a -> let r = P.Request.Action a in Websocket.send ws
   (Sexp.to_string_hum [%sexp (r : P.Request.t)]) | `Replace _ -> assert false) in scene
   ~state:(Bonsai.Var.value state_var) ~dispatch ;; *)

(* let onlineinit ~room_id = let%sub init, set_init = Bonsai.state_opt [%here] (module
   S.Model) in match%sub init with | Some init -> let state_var = Bonsai.Var.create init
   in (* online ~state_var ~room_id *) assert false | None -> let%arr set_init = set_init
   in Vdom.(Node.button ~attr:Attr.(many [ on_click (fun _ -> set_init None) ]) []) ;; *)

let online123 ~state_var ~room_id =
  let%sub ws, set_ws = Bonsai.state_opt [%here] (module Websocket) in
  let state = Bonsai.Var.value state_var in
  let%sub dispatch =
    let%arr ws = ws in
    Effect.of_sync_fun (function
        | `Action a ->
          begin
            match ws with
            | Some ws ->
              let r = P.Request.Action a in
              Websocket.send ws (Sexp.to_string_hum [%sexp (r : P.Request.t)])
            | None -> ()
          end
        | `Replace _ -> assert false)
  in
  let%sub scene = scene ~state ~dispatch in
  let%arr room_id = room_id
  and set_ws = set_ws
  and ws = ws
  and scene = scene in
  Vdom.(
    Node.div
      [ Node.button
          ~attr:
            Attr.(
              many
                [ on_click (fun _ ->
                      match ws with
                      | None ->
                        let w = connect ~var:state_var ~room_id in
                        set_ws (Some w)
                      | Some _ -> Effect.Ignore)
                ])
          [ Node.text "connect" ]
      ; Node.button
          ~attr:
            Attr.(
              many
                [ on_click (fun _ ->
                      match ws with
                      | None ->
                        let%map.Effect str =
                          Effect_lwt.of_lwt_fun
                            (fun () ->
                              let%bind.Lwt _response, body =
                                Cohttp_lwt_jsoo.Client.get
                                  (Uri.of_string ("/room/" ^ Int.to_string room_id))
                              in
                              Cohttp_lwt.Body.to_string body)
                            ()
                        in
                        let model = str |> Sexp.of_string |> [%of_sexp: S.Model.t] in
                        Bonsai.Var.set state_var model
                      | Some _ -> Effect.Ignore)
                ])
          [ Node.text "init" ]
      ; scene
      ])
;;

let online ~room_id =
  let state_var = Bonsai.Var.create (S.Model.init ~g:10.) in
  online123 ~state_var ~room_id
;;

let offline =
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
                     { id = S.Figure2.Id.next ()
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
                     { id = S.Figure2.Id.next ()
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
                     { id = S.Figure2.Id.next ()
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
                     { id = S.Figure2.Id.next ()
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
  scene ~state ~dispatch
;;
