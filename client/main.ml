open Core
open Bonsai_web
open Bonsai.Let_syntax

let box =
  let%sub state, set_state = Bonsai.state [%here] (module String) ~default_model:"" in
  let%arr state = state
  and set_state = set_state in
  ( state
  , Vdom.Node.input
      ~attr:
        (Vdom.Attr.many
           [ Vdom.Attr.on_input (fun _ -> set_state); Vdom.Attr.value state ])
      [] )
;;

let component =
  let%sub a, b = box in
  let%arr b = b
  and a = a in
  Vdom.Node.div [ b; Vdom.Node.text a ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
