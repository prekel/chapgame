open Core
open Bonsai_web
open Bonsai.Let_syntax
open Js_of_ocaml
module Svg = Virtual_dom_svg

let svg_ref = ref None
let pt_ref = ref None

let svg_click_coords (evt : Dom_html.mouseEvent Js.t) =
  match
    evt##.target
    |> Js.Opt.to_option
    |> Option.map ~f:Dom_svg.CoerceTo.element
    |> Option.bind ~f:Js.Opt.to_option
  with
  | Some e ->
    let svg =
      match e##.ownerSVGElement |> Js.some |> Js.Opt.to_option with
      | Some e -> e
      | None -> Dom_svg.CoerceTo.svg e |> Js.Opt.to_option |> Option.value_exn
    in
    let pt =
      match !svg_ref, !pt_ref with
      | Some svg, Some pt when phys_equal svg e##.ownerSVGElement -> pt
      | _ ->
        let pt = svg##createSVGPoint in
        svg_ref := Some e##.ownerSVGElement;
        pt_ref := Some pt;
        pt
    in
    (Obj.magic pt)##.x := evt##.clientX;
    (Obj.magic pt)##.y := evt##.clientY;
    let cursorpt = pt##matrixTransform svg##getScreenCTM##inverse in
    let x = cursorpt##.x in
    let y = cursorpt##.y in
    x, y
  | None -> assert false
;;

module Make
    (C : Engine.Module_types.CONSTS with module N = Float)
    (S : module type of Engine.Scene.Make (C)) =
    struct
  let circle (id, figure) ~body_click =
    let x = S.Values.get_scalar_exn figure.S.Body.values ~var:`x0 in
    let y = S.Values.get_scalar_exn figure.values ~var:`y0 in
    let r = S.Values.get_scalar_exn figure.values ~var:`r in
    let on_click id (evt : Dom_html.mouseEvent Js.t) =
      let sx, sy = svg_click_coords evt in
      let nx = sx -. x in
      let ny = sy -. y in
      body_click id nx ny r
    in
    let open Vdom in
    Svg.Node.circle
      ~key:(S.Body.Id.to_string id)
      ~attr:
        (Attr.many
           [ Svg.Attr.cx x; Svg.Attr.cy y; Svg.Attr.r r; Attr.on_click (on_click id) ])
      []
  ;;

  let line (S.LineSegmentRay.{ p1; p2; kind } as line) =
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
      ~key:(Sexp.to_string [%sexp (line : S.LineSegmentRay.t)])
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

  let point (S.Point.{ x; y } as point) =
    let open Vdom in
    Svg.Node.circle
      ~key:(Sexp.to_string [%sexp (point : S.Point.t)])
      ~attr:(Attr.many [ Svg.Attr.cx x; Svg.Attr.cy y; Svg.Attr.r 1. ])
      []
  ;;

  let actual_wh_var = Bonsai.Var.create None

  (* https://github.com/janestreet/incr_dom/blob/04804a65152a751ca9b795475dbf1159acfc2e4e/example/widget_raw_html/app.ml#L26-L63 *)
  let view_resize_observer =
    let widget_id =
      Type_equal.Id.create
        ~name:"my widget type"
        (fun (_ : ResizeObserver.resizeObserver Js.t * Dom_html.element Js.t) ->
          Sexp.Atom "<resize-observer-widget>")
    in
    Vdom.Node.widget
      ~id:widget_id
      ~init:(fun () ->
        let div = Dom_html.document##createElement (Js.string "div") in
        div##.className := Js.string "resize-div";
        let resize_observer =
          Js_of_ocaml.ResizeObserver.observe
            ~box:(Js.string "content-box")
            ~node:div
            ~f:(fun entries _observer ->
              Js_of_ocaml.Js.to_array entries
              |> Array.to_list
              |> List.iter ~f:(fun entry ->
                     let width =
                       entry##.contentRect##.width
                       |> Js.Optdef.to_option
                       |> Option.value_exn
                     in
                     let height =
                       entry##.contentRect##.height
                       |> Js.Optdef.to_option
                       |> Option.value_exn
                     in
                     Bonsai.Var.set actual_wh_var (Some (width, height))))
            ()
        in
        resize_observer, div)
      ~destroy:(fun resize_observer _dom_node -> resize_observer##disconnect)
      ()
  ;;

  let scene_frame
      ~scene
      ~(body_click : (S.Body.Id.t -> float -> float -> float -> unit Ui_effect.t) Value.t)
      ~time
      ~viewbox
      ~(* ~set_viewbox ~scale *)
       move_viewbox
    =
    let%sub scale, set_scale = Bonsai.state [%here] (module Float) ~default_model:1. in
    let%sub mouse_down_coords, set_mouse_down_coords =
      Bonsai.state_opt
        [%here]
        (module struct
          type t = float * float [@@deriving sexp, equal]
        end)
    in
    let%arr (scene : S.Scene.t) = scene
    and body_click = body_click
    and time = time
    and min_x, min_y, _width, _height = viewbox
    and width, height =
      Bonsai.Var.value actual_wh_var
      |> Bonsai.Value.map ~f:(Option.value ~default:(1000., 1000.))
    and mouse_down_coords = mouse_down_coords
    and set_mouse_down_coords = set_mouse_down_coords
    and move_viewbox = move_viewbox
    and scale = scale
    and set_scale = set_scale in
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
    Node.div
      [ Svg.Node.svg
          ~attr:
            (Attr.many
               [ Attr.on_wheel (fun event ->
                     let (dy : float) = (Js.Unsafe.coerce event)##.deltaY in
                     let nx, ny = svg_click_coords (event :> Dom_html.mouseEvent Js.t) in
                     let scaled = 0.03 *. if Float.is_positive dy then 1. else -1. in
                     let new_scale = scale +. scaled in
                     if Float.(new_scale <= 0.01)
                     then Effect.Ignore
                     else (
                       let c = new_scale /. scale in
                       let x = Float.(nx - ((nx - min_x) * c) - min_x) in
                       let y = Float.(ny - ((ny - min_y) * c) - min_y) in
                       let%bind.Effect () = set_scale new_scale in
                       move_viewbox (x, y)))
               ; Attr.on_mousemove (fun event ->
                     match (Js.Unsafe.coerce event)##.buttons with
                     | 1 ->
                       (match mouse_down_coords with
                       | Some (ox, oy) ->
                         let nx, ny = svg_click_coords event in
                         let%bind.Effect () = set_mouse_down_coords (Some (ox, oy)) in
                         move_viewbox Float.(-(nx - ox), -(ny - oy))
                       | None ->
                         let ox, oy = svg_click_coords event in
                         set_mouse_down_coords (Some (ox, oy)))
                     | _ -> set_mouse_down_coords None)
               ; Svg.Attr.viewbox
                   ~min_x
                   ~min_y
                   ~width:(width *. scale)
                   ~height:(height *. scale)
               ; Svg.Attr.preserve_aspect_ratio ~align:X_min_y_mid ~meet_or_slice:`Meet ()
               ])
          all
      ; view_resize_observer
      ]
  ;;

  let frame_time60 = 1. /. 60.
  let to_prec a = a *. 1000.
  let of_prec a = a /. 1000.

  let scene1 ~model ~time ~body_click ~viewbox ~move_viewbox =
    let%sub scene =
      let%arr model = model
      and time = time in
      model.S.Model.scenes |> S.Scenes.before ~time |> snd
    in
    scene_frame ~scene ~time ~body_click ~viewbox ~move_viewbox
  ;;

  let calc_new_time_every ~model ~time ~set_time ~is_pause ~speed =
    let%sub calc_new_time =
      let%arr is_pause = is_pause
      and speed = speed
      and model = model
      and time = time
      and set_time = set_time in
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
      set_time tm
    in
    Bonsai.Clock.every [%here] (Time_ns.Span.of_sec frame_time60) calc_new_time
  ;;

  let timespeed_box ~title ~value ~buttons ~input =
    let title_ = title
    and value_ = value
    and buttons_ = buttons
    and input_ = input in
    let open Vdom in
    let open Node in
    let open Attr in
    div
      ~attr:(many [ classes [ "box" ] ])
      [ div
          ~attr:(many [ classes [ "columns"; "is-mobile"; "is-gapless" ] ])
          [ div
              ~attr:(many [ classes [ "column"; "is-9" ] ])
              [ h5 ~attr:(many [ classes [ "title"; "is-5" ] ]) [ text title_ ] ]
          ; div
              ~attr:(many [ classes [ "column"; "is-3" ] ])
              [ p [ text (sprintf "%.2f" value_) ] ]
          ]
      ; div
          ~attr:(many [ classes [ "columns"; "is-mobile"; "is-gapless" ] ])
          [ div
              ~attr:(many [ classes [ "column"; "is-9" ] ])
              [ div
                  ~attr:(many [ classes [ "buttons"; "has-addons"; "are-small" ] ])
                  buttons_
              ]
          ; div ~attr:(many [ classes [ "column"; "is-3" ] ]) [ input_ ]
          ]
      ]
  ;;

  let time_panel ~time_changed_manually =
    let%sub time, set_time = Bonsai.state [%here] (module Float) ~default_model:0. in
    let%sub field, set_field = Bonsai.state [%here] (module String) ~default_model:"" in
    let%sub is_focused, set_is_focused =
      Bonsai.state [%here] (module Bool) ~default_model:false
    in
    let%sub set_time =
      let%arr set_time = set_time in
      fun t -> if Float.is_negative t then Effect.Ignore else set_time t
    in
    let%sub set_time =
      let%arr set_time = set_time
      and set_field = set_field
      and is_focused = is_focused in
      if is_focused
      then set_time
      else
        fun t ->
        let%bind.Effect () = set_field (sprintf "%.2f" t) in
        set_time t
    in
    let%sub set_time_manually =
      let%arr set_time = set_time
      and time_changed_manually = time_changed_manually in
      fun new_time ->
        let%bind.Effect () = time_changed_manually new_time in
        set_time new_time
    in
    let%arr time = time
    and set_time = set_time
    and set_time_manually = set_time_manually
    and field = field
    and set_field = set_field
    and set_is_focused = set_is_focused in
    let open Vdom in
    let open Node in
    let open Attr in
    ( time
    , set_time
    , timespeed_box
        ~title:"Time"
        ~value:time
        ~buttons:
          [ button
              ~attr:
                (many
                   [ classes [ "button" ]
                   ; on_click (fun _ -> set_time_manually (time -. 1.))
                   ])
              [ text "-1" ]
          ; button
              ~attr:
                (many [ classes [ "button" ]; on_click (fun _ -> set_time_manually 0.) ])
              [ text "0" ]
          ; button
              ~attr:
                (many
                   [ classes [ "button" ]
                   ; on_click (fun _ -> set_time_manually (time +. 1.))
                   ])
              [ text "+1" ]
          ]
        ~input:
          (input
             ~attr:
               (Attr.many
                  [ classes [ "input"; "is-small" ]
                  ; type_ "text"
                  ; on_focus (fun _ -> set_is_focused true)
                  ; on_blur (fun _ -> set_is_focused false)
                  ; on_input (fun _ s -> set_field s)
                  ; on_change (fun _ a ->
                        try a |> Float.of_string |> set_time_manually with
                        | _ -> Effect.Ignore)
                  ; value_prop field
                  ])
             []) )
  ;;

  let speed_panel ~speed ~set_speed =
    let%arr speed = speed
    and set_speed = set_speed in
    let open Vdom in
    let open Node in
    div
      [ input
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
      ; input
          ~attr:
            (Attr.many
               [ Attr.on_change (fun _ a ->
                     try a |> Float.of_string |> set_speed with
                     | _ -> Effect.Ignore)
               ; Attr.value (Float.to_string speed)
               ])
          []
      ]
  ;;

  let timeoutd_panel ~timeoutd ~set_timeoutd =
    let%arr timeoutd = timeoutd
    and set_timeoutd = set_timeoutd in
    let open Vdom in
    let open Node in
    div
      [ input
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
      ; input
          ~attr:
            (Attr.many
               [ Attr.on_change (fun _ a ->
                     try a |> Float.of_string |> set_timeoutd with
                     | _ -> Effect.Ignore)
               ; Attr.value (Float.to_string timeoutd)
               ])
          []
      ]
  ;;

  let viewbox_panel ~viewbox_init =
    let%sub viewbox, set_viewbox =
      Bonsai_extra.state_dynamic_model
        [%here]
        (module struct
          type t = float * float * float * float [@@deriving sexp, equal]
        end)
        ~model:(`Given viewbox_init)
    in
    let open Vdom in
    let open Node in
    let%arr ((min_x, min_y, width, height) as viewbox) = viewbox
    and set_viewbox = set_viewbox in
    let set_min_x min_x = set_viewbox (min_x, min_y, width, height) in
    let set_min_y min_y = set_viewbox (min_x, min_y, width, height) in
    let set_width width = set_viewbox (min_x, min_y, width, height) in
    let set_height height = set_viewbox (min_x, min_y, width, height) in
    let inp a set =
      input
        ~attr:
          (Attr.many
             [ Attr.on_change (fun _ x ->
                   try x |> Float.of_string |> set with
                   | _ -> Effect.Ignore)
             ; Attr.value (Float.to_string a)
             ])
        []
    in
    ( viewbox
    , (fun (dx, dy) -> set_viewbox (min_x +. dx, min_y +. dy, width, height))
    , div
        [ inp min_x set_min_x
        ; br ()
        ; inp min_y set_min_y
        ; br ()
        ; inp width set_width
        ; br ()
        ; inp height set_height
        ; br ()
        ] )
  ;;

  let scene ~(state : S.Model.t Value.t) ~dispatch ~time_changed_manually =
    let%sub is_pause, set_is_pause =
      Bonsai.state [%here] (module Bool) ~default_model:false
    in
    let%sub speed, set_speed = Bonsai.state [%here] (module Float) ~default_model:1. in
    let%sub timeoutd, set_timeoutd =
      Bonsai.state [%here] (module Float) ~default_model:10.
    in
    let%sub time, set_time, time_panel = time_panel ~time_changed_manually in
    let%sub () = calc_new_time_every ~model:state ~time ~set_time ~is_pause ~speed in
    let%sub text_state, set_text_state =
      Bonsai.state [%here] (module String) ~default_model:""
    in
    let%sub body_click =
      let%arr time = time
      and timeoutd = timeoutd
      and dispatch = dispatch in
      fun id x y r ->
        dispatch
          (`Action
            S.Action.
              { time
              ; action =
                  S.Action.GiveVelocity { id; v0 = Float.(x / r * -200., y / r * -200.) }
              ; timeout = Some Float.(time + timeoutd)
              })
    in
    let%sub viewbox, move_viewbox, viewbox_panel =
      viewbox_panel ~viewbox_init:(Bonsai.Value.return (0., 0., 1000., 1000.))
    in
    let%sub frame = scene1 ~model:state ~time ~body_click ~viewbox ~move_viewbox in
    let%sub speed_panel = speed_panel ~speed ~set_speed in
    let%sub timeoutd_panel = timeoutd_panel ~timeoutd ~set_timeoutd in
    let%arr time = time
    and frame = frame
    and dispatch = dispatch
    and is_pause = is_pause
    and set_is_pause = set_is_pause
    and time_panel = time_panel
    and timeoutd_panel = timeoutd_panel
    and text_state = text_state
    and set_text_state = set_text_state
    and state = state
    and timeoutd = timeoutd
    and speed_panel = speed_panel
    and viewbox_panel = viewbox_panel in
    Vdom.(
      ( Node.div
          [ time_panel
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
          ; speed_panel
          ; Node.br ()
          ; Node.br ()
          ; timeoutd_panel
          ; Node.br ()
          ; viewbox_panel
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
          ]
      , frame ))
  ;;
end
