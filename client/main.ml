open Core
open Bonsai_web
open Bonsai.Let_syntax

let box =
  let%sub state, set_state = Bonsai.state [%here] (module String) ~default_model:"" in
  let%arr state = state
  and set_state = set_state in
  ( float_of_string_opt state
  , Vdom.Node.input
      ~attr:
        (Vdom.Attr.many
           [ Vdom.Attr.on_input (fun _ -> set_state); Vdom.Attr.value state ])
      [] )
;;

let component =
  let%sub a, box_a = box in
  let%sub b, box_b = box in
  let%arr a = a
  and box_a = box_a
  and b = b
  and box_b = box_b in
  let ab =
    Option.Let_syntax.(
      let%bind a = a in
      let%map b = b in
      a +. b)
    |> fun ab -> [%sexp (ab : float option)] |> Sexp.to_string_hum
  in
  Vdom.Node.div [ box_a; box_b; Vdom.Node.text ab ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
