open Core
open Brr

let path () =
  Window.location G.window
  |> Uri.path
  |> Jstr.to_string
  |> String.chop_prefix_if_exists ~prefix:"/"
  |> String.split ~on:'/'
;;

let query () =
  Window.location G.window
  |> Uri.to_jstr
  |> Uri.Params.of_jstr
  |> Uri.Params.to_assoc
  |> List.map ~f:(fun (l, r) -> Jstr.to_string l, Jstr.to_string r)
;;

let set_location path query =
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
  | Ok new_location -> Window.set_location G.window new_location
  | Error error -> Js_of_ocaml.Firebug.console##error error
;;
