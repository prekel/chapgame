open Core
open Lwt.Let_syntax

module Make
    (C : Engine.Module_types.CONSTS)
    (S : module type of Engine.Scene.Make (C)) =
    struct
  module Store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)

  let repo_field = Dream.new_field ()
  let repo_var = ref None

  let middleware inner_handler request =
    match !repo_var with
    | Some repo ->
      Dream.set_field request repo_field repo;
      inner_handler request
    | None -> Dream.empty `OK
  ;;

  let init_store dir =
    let%map repo = Store.Repo.v (Irmin_git.config ~bare:true dir) in
    repo_var := Some repo;
    Lwt.return_unit
  ;;

  let route =
    Dream.scope
      "/replay"
      [ middleware ]
      [ Dream.get "/:replay_id" (fun request ->
            let repo = Dream.field request repo_field in
            let replay_id = Dream.param request "replay_id" in
            let%bind t = Store.of_branch repo replay_id in
            let%bind m = Store.get t [ "replay.scm" ] in
            assert false)
      ]
  ;;
end
