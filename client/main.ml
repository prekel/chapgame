open Core
open Bonsai_web
open Bonsai.Let_syntax
open Js_of_ocaml
module SF = Chapgame.Solver.MakeSolver (Float)

let state () =
  let init =
    [%sexp
      (SF.Polynomial.of_list
         [ SF.Monomial.create ~degree:3 ~coefficient:1.
         ; SF.Monomial.create ~degree:2 ~coefficient:(-2.)
         ; SF.Monomial.create ~degree:1 ~coefficient:(-1.)
         ; SF.Monomial.create ~degree:0 ~coefficient:2.
         ]
        : SF.Polynomial.t)]
    |> Sexp.to_string_hum
  in
  let%sub state, set_state = Bonsai.state [%here] (module String) ~default_model:init in
  let%arr state = state
  and set_state = set_state in
  state, set_state
;;

let polynomial_and_roots ~poly ~eps =
  let roots =
    try Some (SF.PolynomialEquation.roots ~eps poly) with
    | err ->
      Firebug.console##warn (Js.string (Exn.to_string err));
      None
  in
  Vdom.(
    Node.div
      [ Node.pre
          [ Node.text (Sexp.to_string_hum ~indent:2 [%sexp (poly : SF.Polynomial.t)]) ]
      ; Node.pre
          [ Node.text (Sexp.to_string_hum ~indent:2 [%sexp (roots : float list option)]) ]
      ; Node.br ()
      ])
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
      [ Vdom.Node.textarea
          ~attr:
            (Vdom.Attr.many
               [ Vdom.Attr.on_input (fun _ -> set_state); Vdom.Attr.value_prop state ])
          [ Vdom.Node.text state ]
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
    let d = SF.Polynomial.derivative poly in
    if SF.Polynomial.degree d = 0 then [ poly; d ] else poly :: derivatives d
  in
  Vdom.(
    Node.div
      [ box_a
      ; Node.div
          (match a with
          | Some a ->
            derivatives a
            |> List.map ~f:(fun poly -> polynomial_and_roots ~poly ~eps:1e-7)
          | None -> [])
      ])
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component
;;
