open Core
open Lwt.Syntax

(* Irmin store with string contents *)
module Store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)

(* Database configuration *)
let config = Irmin_git.config ~bare:true "/tmp/irmin/test"

(* Commit author *)
let author = "Example <example@example.com>"

(* Commit information *)
let info fmt = Irmin_unix.info fmt

let main =
  (* Open the repo *)
  let* repo = Store.Repo.v config in
  (* Load the main branch *)
  let* t = Store.of_branch repo "init" in
  (* Set key "foo/bar" to "testing 123" *)
  let* () =
    Store.set_exn t ~info:(info "Updating2 foo/bar") [ "foo"; "bar" ] "testing 1233"
  in
  let* head = Store.Head.get t in
  let* () = Store.Branch.set repo "develop1" head in
  let* _w =
    Store.watch_key t [ "foo"; "bar" ] (function
        | `Updated _u ->
          printf "Updated\n";
          Lwt.return_unit
        | _ -> assert false)
  in
  Lwt_unix.sleep 1000.
;;

(* Run the program *)
let () = Lwt_main.run main
