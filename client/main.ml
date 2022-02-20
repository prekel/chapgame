open Core
open Bonsai_web
open Bonsai.Let_syntax
module SF = Chapgame.Polynomial.MakeSolver (Float)

let state () =
  let init =
    [%sexp
      (SF.Polynomial.of_list
         [ SF.Monomial.create ~degree:2 ~coefficient:3.
         ; SF.Monomial.create ~degree:1 ~coefficient:(-2.)
         ]
        : SF.Polynomial.t)]
    |> Sexp.to_string_hum
  in
  let%sub state, set_state = Bonsai.state [%here] (module String) ~default_model:init in
  let%arr state = state
  and set_state = set_state in
  state, set_state
;;

let box () =
  let%sub state, set_state = state () in
  let%arr state = state
  and set_state = set_state in
  ( (try
       state
       |> Sexp.of_string
       |> SF.Polynomial.t_of_sexp
       |> SF.Polynomial.normalize
       |> Option.some
     with
    | _ -> None)
  , Vdom.Node.div
      [ Vdom.Node.input
          ~attr:
            (Vdom.Attr.many
               [ Vdom.Attr.on_input (fun _ -> set_state); Vdom.Attr.value state ])
          []
      ; Vdom.Node.text state
      ; Vdom.Node.button
          ~attr:
            (Vdom.Attr.on_click (fun _ ->
                 set_state (Sexp.of_string state |> Sexp.to_string_hum ~indent:2)))
          [ Vdom.Node.text "format" ]
      ] )
;;

let component =
  let%sub a, box_a = box () in
  let%arr a = a
  and box_a = box_a in
  let ab =
    match a with
    | Some a ->
      Sexp.to_string_hum ~indent:2 [%sexp (SF.Polynomial.derivative a : SF.Polynomial.t)]
    | _ -> ""
  in
  Vdom.Node.div [ box_a; Vdom.Node.text ab ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
