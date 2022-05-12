(* open Core
open Lwt.Let_syntax

module Make
    (C : Engine.Module_types.CONSTS)
    (S : module type of Engine.Scene.Make (C)) =
    struct

  let repo_field = Dream.new_field ()
  let repo_var = ref None

  let middleware inner_handler request =
    match !repo_var with
    | Some repo ->
      Dream.set_field request repo_field repo;
      inner_handler request
    | None -> Dream.empty `Internal_Server_Error
  ;;



  let route =
    Dream.scope
      "/replay"
      [ middleware ]
      [ Dream.get "/:replay_id" (fun request ->
            match Dream.field request repo_field with
            | Some repo ->
              (* let replay_id = Dream.param request "replay_id" in
              let%bind t = Store.of_branch repo replay_id in
              let%bind _m = Store.get t [ "replay.scm" ] in *)
              assert false
            | None -> Dream.empty `Internal_Server_Error)
      ]
  ;;
end *)
