open Core
open Brr
open Js_of_ocaml
open Bonsai_web
open Bonsai.Let_syntax

let path () =
  Window.location G.window
  |> Uri.path
  |> Jstr.to_string
  |> String.chop_prefix_if_exists ~prefix:"/"
  |> String.split ~on:'/'
;;

let query () =
  Window.location G.window
  |> Uri.query
  |> Uri.Params.of_jstr
  |> Uri.Params.to_assoc
  |> List.map ~f:(fun (l, r) -> Jstr.to_string l, Jstr.to_string r)
;;

let path_var = Bonsai.Var.create (path ())
let query_var = Bonsai.Var.create (query ())

let push =
  Effect.of_sync_fun (fun (path, query) ->
      match
        Uri.with_uri
          ~path:(path |> String.concat ~sep:"/" |> Jstr.of_string)
          ~query:
            (query
            |> List.map ~f:(fun (l, r) -> sprintf "%s=%s" l r)
            |> String.concat ~sep:"&"
            |> Jstr.of_string)
          (Window.location G.window)
      with
      | Ok new_location ->
        Bonsai.Var.set path_var path;
        Bonsai.Var.set query_var query;
        Window.History.push_state (Window.history G.window) ~uri:new_location
      | Error error -> Js_of_ocaml.Firebug.console##error error)
;;

let use () =
  Dom_html.window##.onpopstate
    := Dom.handler (fun _ ->
           Bonsai.Var.set path_var (path ());
           Bonsai.Var.set query_var (query ());
           Js._false);
  let%arr path = Bonsai.Var.value path_var
  and query = Bonsai.Var.value query_var in
  path, query
;;
