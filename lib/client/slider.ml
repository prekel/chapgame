(* open Core *)
open Bonsai_web
open Bonsai.Let_syntax

let component ~value ~value_changed inner =
  let%sub inner = inner in
  let%arr value = value
  and value_changed = value_changed
  and inner = inner in
  Vdom.(
    Node.div
      [ Node.input
          ~attr:
            (Attr.many
               [ Attr.type_ "range"
               ; Attr.classes [ "progress"; "is-primary" ]
               ; Attr.value (Float.to_string value)
               ; Attr.max 100.
               ; Attr.on_click (fun event ->
                     let new_value = event##.offsetX |> Int.to_float in
                     Js_of_ocaml.Firebug.console##log event;
                     value_changed new_value)
               ])
          []
      ; inner
      ])
;;
