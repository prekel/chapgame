open Core
open Bonsai_web
open Bonsai.Let_syntax
open Js_of_ocaml
module SF = Chapgame.Solver.MakeSolver (Float)

let eps = 1e-5

let state () =
  let init =
    [%sexp
      (SF.Polynomial.of_list [ 3, 1.; 2, -2.; 1, -1.; 0, 2. ] ~eps : SF.Polynomial.t)]
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
      ; Node.text (SF.Polynomial.to_string_hum poly)
      ; Node.pre
          [ Node.text (Sexp.to_string_hum ~indent:2 [%sexp (roots : float list option)]) ]
      ; Node.br ()
      ])
;;

let box () =
  let%sub state, set_state = state () in
  let%arr state = state
  and set_state = set_state in
  ( (try state |> Sexp.of_string |> SF.Polynomial.t_of_sexp |> Option.some with
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

module ExprDemo = struct
  module Expr = Chapgame.Expr.Make (String) (Nothing) (Float)
  module Formula = Chapgame.Formula.Make (String) (Nothing) (Float) (Expr) (SF)

  let component =
    let%sub values, set_values =
      Bonsai.state
        [%here]
        (module struct
          type t = (string * float) list [@@deriving sexp, equal]
        end)
        ~default_model:[ "x", 1.; "v_x", 1.; "v_y", 4. ]
    in
    let%sub exprs, set_exprs =
      Bonsai.state
        [%here]
        (module struct
          type t = Expr.t_scalar list [@@deriving sexp]

          let equal = List.equal Expr.equal
        end)
        ~default_model:Expr.Syntax.[ scalar_var "x"; vector_y (vector_var "v_x" "v_y") ]
    in
    let%arr values = values
    and set_values = set_values
    and exprs = exprs
    and set_exprs = set_exprs in
    Vdom.(
      Node.div
        [ Node.textarea
            ~attr:
              (Attr.many
                 [ Attr.on_change (fun _ s ->
                       set_values
                         (List.Assoc.t_of_sexp
                            String.t_of_sexp
                            float_of_sexp
                            (Sexp.of_string s)))
                 ; Attr.value_prop
                     (Sexp.to_string_hum [%sexp (values : (string * float) list)])
                 ])
            []
        ; Node.textarea
            ~attr:
              (Attr.many
                 [ Attr.on_change (fun _ s ->
                       set_exprs (List.t_of_sexp Expr.t_scalar_of_sexp (Sexp.of_string s)))
                 ; Attr.value_prop
                     (Sexp.to_string_hum [%sexp (exprs : Expr.t_scalar list)])
                 ])
            []
        ; Node.pre
            [ Node.text
                (try
                   Sexp.to_string
                     [%sexp
                       (List.map exprs ~f:(fun e ->
                            Expr.calc
                              ~values:(fun k ->
                                List.Assoc.find_exn ~equal:String.equal values k)
                              ~scoped_values:never_returns
                              (module Float)
                              e)
                         : float list)]
                 with
                | _ -> "")
            ]
        ])
  ;;
end

let component =
  let%sub a, box_a = box () in
  let%sub demo2 = ExprDemo.component in
  let%arr a = a
  and box_a = box_a
  and demo2 = demo2 in
  let rec derivatives poly =
    let d = SF.Polynomial.derivative poly in
    if SF.Polynomial.degree d = 0 then [ poly; d ] else poly :: derivatives d
  in
  Vdom.(
    Node.div
      [ demo2
      ; Node.br ()
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

let component1 = Scene.scene

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" component1
;;
