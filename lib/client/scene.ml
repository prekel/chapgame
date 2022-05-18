open Core
open Bonsai_web
open Bonsai.Let_syntax
open Js_of_ocaml
module Svg = Virtual_dom_svg

let format_float = sprintf "%.2f"
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

let frame_time60 = 1. /. 60.

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
        ~attr:
          (many [ classes [ "columns"; "is-mobile"; "is-gapless"; "nomarginbottom" ] ])
        [ div
            ~attr:(many [ classes [ "column"; "is-9" ] ])
            [ h5 ~attr:(many [ classes [ "title"; "is-5" ] ]) [ text title_ ] ]
        ; div
            ~attr:(many [ classes [ "column"; "is-3" ] ])
            [ p [ text (format_float value_) ] ]
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

let set_value_and_speed_if_not_focused ~set_value ~is_focused ~set_field =
  let%arr set_value = set_value
  and set_field = set_field
  and is_focused = is_focused in
  if is_focused
  then set_value
  else
    fun t ->
    let%bind.Effect () = set_field (format_float t) in
    set_value t
;;

let set_value_manually ~value_changed_manually ~set_value =
  let%arr set_value = set_value
  and value_changed_manually = value_changed_manually in
  fun new_value ->
    let%bind.Effect () = value_changed_manually new_value in
    set_value new_value
;;

let interactive_input ~value ~set_manually =
  let%sub init = Bonsai_extra.freeze [%here] (module Float) value in
  let%sub field, set_field =
    Bonsai_extra.state_dynamic_model
      [%here]
      (module String)
      ~model:(`Given (Bonsai.Value.map init ~f:format_float))
  in
  let%sub is_focused, set_is_focused =
    Bonsai.state [%here] (module Bool) ~default_model:false
  in
  let%sub value_changed =
    let%arr is_focused = is_focused
    and set_field = set_field in
    fun v -> if is_focused then Effect.Ignore else set_field (format_float v)
  in
  let%sub () =
    Bonsai.Edge.on_change [%here] (module Float) value ~callback:value_changed
  in
  let%arr field = field
  and set_field = set_field
  and set_is_focused = set_is_focused
  and set_manually = set_manually in
  let open Vdom in
  let open Node in
  let open Attr in
  input
    ~attr:
      (Attr.many
         [ classes [ "input"; "is-small" ]
         ; type_ "text"
         ; on_focus (fun _ -> set_is_focused true)
         ; on_blur (fun _ -> set_is_focused false)
         ; on_input (fun _ s -> set_field s)
         ; on_change (fun _ a ->
               try a |> Float.of_string |> set_manually with
               | _ -> Effect.Ignore)
         ; value_prop field
         ])
    []
;;

let time_panel ~time_changed_manually =
  let%sub time, set_time = Bonsai.state [%here] (module Float) ~default_model:0. in
  let%sub set_time =
    let%arr set_time = set_time in
    fun t -> if Float.is_negative t then Effect.Ignore else set_time t
  in
  let%sub set_time_manually =
    set_value_manually ~value_changed_manually:time_changed_manually ~set_value:set_time
  in
  let%sub interactve_input =
    interactive_input ~value:time ~set_manually:set_time_manually
  in
  let%arr time = time
  and set_time = set_time
  and set_time_manually = set_time_manually
  and interactve_input = interactve_input in
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
      ~input:interactve_input )
;;

let speed_panel ~speed_changed_manually =
  let%sub speed, set_speed = Bonsai.state [%here] (module Float) ~default_model:1. in
  let%sub set_speed_manually =
    set_value_manually ~value_changed_manually:speed_changed_manually ~set_value:set_speed
  in
  let%sub paused_speed, set_paused_speed = Bonsai.state_opt [%here] (module Float) in
  let%sub interactve_input =
    interactive_input ~value:speed ~set_manually:set_speed_manually
  in
  let%arr speed = speed
  and set_speed = set_speed
  and set_speed_manually = set_speed_manually
  and paused_speed = paused_speed
  and set_paused_speed = set_paused_speed
  and interactve_input = interactve_input in
  let open Vdom in
  let open Node in
  let open Attr in
  ( speed
  , set_speed
  , timespeed_box
      ~title:"Speed"
      ~value:speed
      ~buttons:
        [ button
            ~attr:
              (many
                 [ classes [ "button" ]
                 ; on_click (fun _ -> set_speed_manually (speed -. 0.1))
                 ])
            [ text "-0.1" ]
        ; button
            ~attr:
              (many
                 [ classes [ "button" ]; on_click (fun _ -> set_speed_manually (-1.)) ])
            [ text "-1x" ]
        ; button
            ~attr:
              (many
                 [ classes [ "button" ]
                 ; on_click (fun _ ->
                       match paused_speed with
                       | Some paused_speed when Float.(speed = 0.) ->
                         let%bind.Effect () = set_paused_speed None in
                         set_speed_manually paused_speed
                       | _ ->
                         let%bind.Effect () = set_paused_speed (Some speed) in
                         set_speed_manually 0.)
                 ])
            [ span
                ~attr:(class_ "icon")
                [ Node.create
                    "i"
                    ~attr:
                      (classes
                         (match paused_speed with
                         | Some _ when Float.(speed = 0.) -> [ "fas"; "fa-play" ]
                         | _ -> [ "fas"; "fa-pause" ]))
                    []
                ]
            ]
        ; button
            ~attr:
              (many [ classes [ "button" ]; on_click (fun _ -> set_speed_manually 1.) ])
            [ text "1x" ]
        ; button
            ~attr:
              (many
                 [ classes [ "button" ]
                 ; on_click (fun _ -> set_speed_manually (speed +. 0.1))
                 ])
            [ text "+0.1" ]
        ]
      ~input:interactve_input )
;;

let calc_new_time_every ~timeout ~time ~set_time ~speed ~timeout_reached =
  let%sub calc_new_time =
    let%arr speed = speed
    and timeout = timeout
    and time = time
    and set_time = set_time
    and timeout_reached = timeout_reached in
    let open Float in
    let new_time =
      match time + (frame_time60 * speed) with
      | nt when nt < 0. -> 0.
      | nt -> nt
    in
    match timeout with
    | Some timeout when Float.(timeout < new_time) -> timeout_reached timeout
    | _ -> set_time new_time
  in
  Bonsai.Clock.every [%here] (Time_ns.Span.of_sec frame_time60) calc_new_time
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

  let scene_frame
      ~scene
      ~(body_click : (S.Body.Id.t -> float -> float -> float -> unit Ui_effect.t) Value.t)
      ~time
    =
    let%sub scale, set_scale = Bonsai.state [%here] (module Float) ~default_model:1. in
    let%sub mouse_down_coords, set_mouse_down_coords =
      Bonsai.state_opt
        [%here]
        (module struct
          type t = float * float [@@deriving sexp, equal]
        end)
    in
    let%sub min_x, set_min_x = Bonsai.state [%here] (module Float) ~default_model:0. in
    let%sub min_y, set_min_y = Bonsai.state [%here] (module Float) ~default_model:0. in
    let%sub move_viewbox =
      let%arr min_x = min_x
      and set_min_x = set_min_x
      and min_y = min_y
      and set_min_y = set_min_y in
      fun (dx, dy) ->
        let%bind.Effect () = set_min_x (min_x +. dx) in
        set_min_y (min_y +. dy)
    in
    let%arr (scene : S.Scene.t) = scene
    and body_click = body_click
    and time = time
    and min_x = min_x
    and min_y = min_y
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

  let box ~title:title_ inner =
    let open Vdom in
    let open Node in
    let open Attr in
    div
      ~attr:(many [ classes [ "box"; "nopaddinglr" ] ])
      [ h5 ~attr:(classes [ "is-5"; "title"; "paddinglr" ]) [ text title_ ]; inner ]
  ;;

  let export_import_clear ~model ~set_model =
    let%sub modal_is_open, set_modal_is_open =
      Bonsai.state [%here] (module Bool) ~default_model:false
    in
    let%sub text_state, set_text_state =
      Bonsai.state [%here] (module String) ~default_model:""
    in
    let%sub set_modal_is_open =
      let%arr set_text_state = set_text_state
      and set_modal_is_open = set_modal_is_open
      and model = model in
      function
      | true ->
        let%bind.Effect () =
          model |> [%sexp_of: S.Model.t] |> Sexp.to_string_hum ~indent:2 |> set_text_state
        in
        set_modal_is_open true
      | false ->
        let%bind.Effect () = set_text_state "" in
        set_modal_is_open false
    in
    let%arr modal_is_open = modal_is_open
    and set_modal_is_open = set_modal_is_open
    and text_state = text_state
    and set_text_state = set_text_state
    and set_model = set_model in
    let open Vdom in
    let open Node in
    let open Attr in
    box
      ~title:"State"
      (div
         ~attr:(classes [ "columns"; "container"; "is-fluid"; "is-mobile" ])
         [ div
             ~attr:(classes [ "column" ])
             [ button
                 ~attr:
                   (many
                      [ classes [ "button"; "is-fullwidth" ]
                      ; on_click (fun _ -> set_modal_is_open true)
                      ])
                 [ text "Copy/Paste" ]
             ]
         ; div
             ~attr:(classes [ "column" ])
             [ button
                 ~attr:
                   (many
                      [ classes [ "button"; "is-fullwidth" ]
                      ; on_click (fun _ -> set_model @@ S.Model.init ~g:10.)
                      ])
                 [ text "Clear" ]
             ]
         ; (if modal_is_open
           then
             div
               ~attr:(classes [ "modal"; "is-active" ])
               [ div
                   ~attr:
                     (many
                        [ class_ "modal-background"
                        ; on_click (fun _ -> set_modal_is_open false)
                        ])
                   []
               ; div
                   ~attr:(many [ class_ "modal-card" ])
                   [ header
                       ~attr:(many [ class_ "modal-card-head" ])
                       [ p
                           ~attr:(many [ class_ "modal-card-title" ])
                           [ text "Current state as text:" ]
                       ; button
                           ~attr:
                             (many
                                [ class_ "delete"
                                ; on_click (fun _ -> set_modal_is_open false)
                                ])
                           []
                       ]
                   ; section
                       ~attr:(class_ "modal-card-body")
                       [ textarea
                           ~attr:
                             (many
                                [ classes [ "textarea"; "is-small" ]
                                ; on_focus
                                    (Effect.of_sync_fun (fun event ->
                                         let this =
                                           event##.target
                                           |> Js.Opt.to_option
                                           |> Option.value_exn
                                           |> Js_of_ocaml.Dom_html.CoerceTo.textarea
                                           |> Js.Opt.to_option
                                           |> Option.value_exn
                                         in
                                         this##select))
                                ; on_change (fun _ -> set_text_state)
                                ])
                           [ text text_state ]
                       ]
                   ; footer
                       ~attr:(class_ "modal-card-foot")
                       [ button
                           ~attr:
                             (many
                                [ classes [ "button"; "is-success" ]
                                ; on_click (fun _ ->
                                      let%bind.Effect () =
                                        text_state
                                        |> Sexp.of_string
                                        |> [%of_sexp: S.Model.t]
                                        |> set_model
                                      in
                                      set_modal_is_open false)
                                ])
                           [ text "OK" ]
                       ; button
                           ~attr:
                             (many
                                [ classes [ "button" ]
                                ; on_click (fun _ -> set_modal_is_open false)
                                ])
                           [ text "Cancel" ]
                       ]
                   ]
               ]
           else none)
         ])
  ;;

  let bodies_table ~scene ~time ~remove_body =
    let%arr scene = scene
    and time = time
    and remove_body = remove_body in
    let bodies =
      scene.S.Scene.bodies
      |> S.Scene.Figures.calc ~t:(time -. scene.time) ~global_values:scene.global_values
    in
    let open Vdom in
    let open Node in
    let open Attr in
    let body_tr body =
      let id = body.S.Body.id |> S.Body.Id.to_string in
      let x = S.Values.get_scalar_exn body.values ~var:`x0 in
      let y = S.Values.get_scalar_exn body.values ~var:`y0 in
      let mu = S.Values.get_scalar_exn body.values ~var:`mu in
      let m = S.Values.get_scalar_exn body.values ~var:`m in
      let r = S.Values.get_scalar_exn body.values ~var:`r in
      let v_x = S.Values.get_scalar_exn body.values ~var:`v0_x in
      let v_y = S.Values.get_scalar_exn body.values ~var:`v0_y in
      let module V = Engine.Vector.Make (Float) in
      let calc =
        S.Expr.calc
          ~values:(S.Values.to_function body.values)
          ~scoped_values:(S.Values.global_to_scoped scene.global_values)
          (module V)
      in
      let ((a_x, a_y) as a_vec) = calc S.Rule.Exprs.a_vec in
      let v_len = V.len (v_x, v_y) in
      let a_len = V.len a_vec in
      tr
        ~key:id
        [ td [ text id ]
        ; td [ text (format_float x) ]
        ; td [ text (format_float y) ]
        ; td [ text (format_float mu) ]
        ; td [ text (format_float m) ]
        ; td [ text (format_float r) ]
        ; td [ text (format_float v_x) ]
        ; td [ text (format_float v_y) ]
        ; td [ text (format_float v_len) ]
        ; td [ text (format_float a_x) ]
        ; td [ text (format_float a_y) ]
        ; td [ text (format_float a_len) ]
        ; td
            [ button
                ~attr:(many [ class_ "delete"; on_click (fun _ -> remove_body body.id) ])
                []
            ]
        ]
    in
    div
      ~attr:(many [ classes [ "box"; "nopaddinglr" ] ])
      [ h5 ~attr:(classes [ "is-5"; "title"; "paddinglr" ]) [ text "Bodies" ]
      ; div
          ~attr:(many [ classes [ "table-container" ] ])
          [ table
              ~attr:(many [ classes [ "table"; "is-narrow"; "is-fullwidth" ] ])
              [ thead
                  [ tr
                      [ th [ p [ text "id" ] ]
                      ; th [ p [ text "x" ] ]
                      ; th [ p [ text "y" ] ]
                      ; th [ p [ text "μ" ] ]
                      ; th [ p [ text "m" ] ]
                      ; th [ p [ text "r" ] ]
                      ; th [ p [ text "v"; Node.create "sub" [ text "x" ] ] ]
                      ; th [ p [ text "v"; Node.create "sub" [ text "y" ] ] ]
                      ; th [ p [ text "|v|" ] ]
                      ; th [ p [ text "a"; Node.create "sub" [ text "x" ] ] ]
                      ; th [ p [ text "a"; Node.create "sub" [ text "y" ] ] ]
                      ; th [ p [ text "|a|" ] ]
                      ; th ~attr:(class_ "deletetd") []
                      ]
                  ]
              ; Node.create "tfoot" []
              ; tbody
                  (bodies
                  |> S.Scene.Figures.to_sequence
                  |> Sequence.map ~f:snd
                  |> Sequence.map ~f:body_tr
                  |> Sequence.to_list)
              ]
          ]
      ]
  ;;

  let lines_table ~lines ~remove_line =
    let%arr lines = lines
    and remove_line = remove_line in
    let open Vdom in
    let open Node in
    let open Attr in
    let line_tr line =
      tr
        [ td [ text (format_float line.S.LineSegmentRay.p1.x) ]
        ; td [ text (format_float line.p1.y) ]
        ; td [ text (format_float line.p2.x) ]
        ; td [ text (format_float line.p2.y) ]
        ; td
            [ p
                ~attr:(many [ style (Css_gen.font_size (`Em_float 0.85)) ])
                [ text
                    (match line.kind with
                    | `Segment -> "Segment"
                    | `Line -> "Line"
                    | `Ray -> "Ray")
                ]
            ]
        ; td
            [ button
                ~attr:(many [ class_ "delete"; on_click (fun _ -> remove_line line) ])
                []
            ]
        ]
    in
    div
      ~attr:(many [ classes [ "box"; "nopaddinglr" ] ])
      [ h5 ~attr:(classes [ "is-5"; "title"; "paddinglr" ]) [ text "Lines" ]
      ; div
          ~attr:(many [ classes [] ])
          [ table
              ~attr:(many [ classes [ "table"; "is-narrow"; "is-fullwidth" ] ])
              [ thead
                  [ tr
                      [ th [ p [ text "P1"; Node.create "sub" [ text "x" ] ] ]
                      ; th [ p [ text "P1"; Node.create "sub" [ text "y" ] ] ]
                      ; th [ p [ text "P2"; Node.create "sub" [ text "x" ] ] ]
                      ; th [ p [ text "P2"; Node.create "sub" [ text "y" ] ] ]
                      ; th [ p [ text "kind" ] ]
                      ; th ~attr:(class_ "deletetd") []
                      ]
                  ]
              ; Node.create "tfoot" []
              ; tbody (lines |> List.map ~f:line_tr)
              ]
          ]
      ]
  ;;

  let points_table ~points ~remove_point =
    let%arr points = points
    and remove_point = remove_point in
    let open Vdom in
    let open Node in
    let open Attr in
    let point_tr point =
      tr
        [ td [ text (format_float point.S.Point.x) ]
        ; td [ text (format_float point.y) ]
        ; td
            [ button
                ~attr:(many [ class_ "delete"; on_click (fun _ -> remove_point point) ])
                []
            ]
        ]
    in
    div
      ~attr:(many [ classes [ "box"; "nopaddinglr" ] ])
      [ h5 ~attr:(classes [ "is-5"; "title"; "paddinglr" ]) [ text "Points" ]
      ; div
          ~attr:(many [ classes [ "table-container" ] ])
          [ table
              ~attr:(many [ classes [ "table"; "is-narrow"; "is-fullwidth" ] ])
              [ thead
                  [ tr
                      [ th [ p [ text "x" ] ]
                      ; th [ p [ text "y" ] ]
                      ; th ~attr:(class_ "deletetd") []
                      ]
                  ]
              ; Node.create "tfoot" []
              ; tbody (points |> List.map ~f:point_tr)
              ]
          ]
      ]
  ;;

  let scene
      ~(model : S.Model.t Value.t)
      ~dispatch
      ~time_changed_manually
      ~speed_changed_manually
    =
    let%sub timeoutd, _set_timeoutd =
      Bonsai.state [%here] (module Float) ~default_model:10.
    in
    let%sub time, set_time, time_panel = time_panel ~time_changed_manually in
    let%sub speed, _, speed_panel = speed_panel ~speed_changed_manually in
    let%sub timeout_reached =
      let%arr dispatch = dispatch
      and timeoutd = timeoutd in
      fun reached_timeout ->
        `Action
          S.Action.
            { time = reached_timeout
            ; action = Empty
            ; until = `Time Float.(reached_timeout + timeoutd)
            }
        |> dispatch
    in
    let%sub () =
      calc_new_time_every
        ~timeout:(Bonsai.Value.map model ~f:(fun { timeout; _ } -> timeout))
        ~time
        ~set_time
        ~speed
        ~timeout_reached
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
              ; until = `Time Float.(time + timeoutd)
              })
    in
    let%sub scene =
      let%arr model = model
      and time = time in
      model.S.Model.scenes |> S.Scenes.before ~time |> snd
    in
    let%sub frame = scene_frame ~scene ~body_click ~time in
    let%sub export_import_clear =
      export_import_clear
        ~model
        ~set_model:
          (Bonsai.Value.map dispatch ~f:(fun dispatch a -> `Replace a |> dispatch))
    in
    let%sub remove_body =
      let%arr dispatch = dispatch
      and time = time
      and timeoutd = timeoutd in
      fun id ->
        `Action
          S.Action.
            { time
            ; action = S.Action.RemoveBody id
            ; until = `Time  Float.(time + timeoutd)
            }
        |> dispatch
    in
    let%sub bodies_table = bodies_table ~scene ~time ~remove_body in
    let%sub remove_line =
      let%arr dispatch = dispatch
      and time = time
      and timeoutd = timeoutd in
      fun line ->
        `Action
          S.Action.
            { time
            ; action = S.Action.RemoveLine line
            ; until = `Time  Float.(time + timeoutd)
            }
        |> dispatch
    in
    let%sub lines_table =
      lines_table
        ~lines:(Bonsai.Value.map scene ~f:(fun scene -> scene.lines |> S.Lines.to_list))
        ~remove_line
    in
    let%sub remove_point =
      let%arr dispatch = dispatch
      and time = time
      and timeoutd = timeoutd in
      fun point ->
        `Action
          S.Action.
            { time
            ; action = S.Action.RemovePoint point
            ; until = `Time  Float.(time + timeoutd)
            }
        |> dispatch
    in
    let%sub points_table =
      points_table
        ~points:
          (Bonsai.Value.map scene ~f:(fun scene -> scene.points |> S.Points.to_list))
        ~remove_point
    in
    let%arr frame = frame
    and time_panel = time_panel
    and speed_panel = speed_panel
    and export_import_clear = export_import_clear
    and bodies_table = bodies_table
    and lines_table = lines_table
    and points_table = points_table in
    let open Vdom in
    let open Node in
    let open Attr in
    ( div
        ~attr:(classes [])
        [ export_import_clear
        ; time_panel
        ; speed_panel
        ; bodies_table
        ; lines_table
        ; points_table
        ]
    , frame )
  ;;
end
