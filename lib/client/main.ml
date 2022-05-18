(* open Core
open Bonsai_web
open Bonsai.Let_syntax
open Js_of_ocaml
module SF = Solver.All.Make (Float)

let eps = 1e-5

let state () =
  let init =
    [%sexp (SF.P.of_list [ 3, 1.; 2, -2.; 1, -1.; 0, 2. ] ~eps : SF.P.t)]
    |> Sexp.to_string_hum
  in
  let%sub state, set_state = Bonsai.state [%here] (module String) ~default_model:init in
  let%arr state = state
  and set_state = set_state in
  state, set_state
;;

let polynomial_and_roots ~poly ~eps =
  let roots =
    try Some (SF.PE.roots ~eps poly) with
    | err ->
      Firebug.console##warn (Js.string (Exn.to_string err));
      None
  in
  Vdom.(
    Node.div
      [ Node.pre [ Node.text (Sexp.to_string_hum ~indent:2 [%sexp (poly : SF.P.t)]) ]
      ; Node.text (SF.P.to_string_hum poly)
      ; Node.pre
          [ Node.text (Sexp.to_string_hum ~indent:2 [%sexp (roots : float list option)]) ]
      ; Node.br ()
      ])
;;

let box () =
  let%sub state, set_state = state () in
  let%arr state = state
  and set_state = set_state in
  ( (try state |> Sexp.of_string |> SF.P.t_of_sexp |> Option.some with
    | _ -> None)
  , Vdom.Node.div
      [ Vdom.Node.textarea
          ~attr:
            (Vdom.Attr.many
               [ Vdom.Attr.on_input (fun _ -> set_state); Vdom.Attr.value_prop state ])
          [ Vdom.Node.text state ]
      ; Vdom.Node.button
          ~attr:
            (Vdom.Attr.on_click (fun _ ->
                 set_state {|((0 60) (1 43) (2 -21) (3 -3) (4 1))|}))
          [ Vdom.Node.text "-4 -1 3 5" ]
      ; Vdom.Node.button
          ~attr:
            (Vdom.Attr.on_click (fun _ ->
                 set_state {|((0 -24) (1 2) (2 17) (3 -8) (4 1))|}))
          [ Vdom.Node.text "-1 2 3 4" ]
      ; Vdom.Node.button
          ~attr:(Vdom.Attr.on_click (fun _ -> set_state {|((0 2) (1 -1) (2 -2) (3 1))|}))
          [ Vdom.Node.text "-1 1 2" ]
        (* ; Vdom.Node.pre [ Vdom.Node.code [ Vdom.Node.text state ] ] *)
        (* ; Vdom.Node.button ~attr: (Vdom.Attr.on_click (fun _ -> set_state
           (Sexp.of_string state |> Sexp.to_string_hum ~indent:2))) [ Vdom.Node.text
           "format" ] ] ) *)
      ] )
;;

let component =
  let%sub a, box_a = box () in
  let%arr a = a
  and box_a = box_a in
  let rec derivatives poly =
    let d = SF.P.derivative poly in
    if SF.P.degree d = 0 then [ poly; d ] else poly :: derivatives d
  in
  Vdom.(
    Node.div
      [ Node.br ()
      ; Node.br ()
      ; Node.br ()
      ; Node.br ()
      ; box_a
      ; Node.div
          (match a with
          | Some a ->
            derivatives a
            |> List.map ~f:(fun poly -> polynomial_and_roots ~poly ~eps:1e-7)
          | None -> [])
      ])
;;

module Scene = Scene.Make ((val Chapgame.Utils.make_consts ~eps:1e-6))

let component1 = Scene.scene

module WebSocketTest = struct
  let component ~ws ~msgs =
    let%sub inputtext, set_inputtext =
      Bonsai.state [%here] (module String) ~default_model:""
    in
    let%arr msgs = msgs
    and inputtext = inputtext
    and set_inputtext = set_inputtext in
    Vdom.(
      Node.div
        [ Node.div
            [ Node.input
                ~attr:
                  Attr.(
                    many
                      [ type_ "submit"
                      ; value "Send"
                      ; on_click (fun _ ->
                            Effect.of_sync_fun
                              (fun () -> Websocket.send ws ~msg:inputtext)
                              ())
                      ])
                []
            ; Node.input
                ~attr:
                  Attr.(
                    many
                      [ type_ "text"; value inputtext; on_input (fun _ -> set_inputtext) ])
                []
            ]
        ; Node.div (List.rev_map msgs ~f:(fun msg -> Node.div [ Node.text msg ]))
        ])
  ;;
end

let main () : _ Start.Handle.t =
  Start.start
    Start.Result_spec.just_the_view
    ~bind_to_element_with_id:"app"
    (Scene.online ~room_id:(Value.return 0))
;; *)