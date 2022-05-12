open Core
open Lwt.Let_syntax

module Make
    (C : Engine.Module_types.CONSTS)
    (S : module type of Engine.Scene.Make (C)) =
    struct
  let middleware = assert false

  let route =
    Dream.scope
      "/replay"
      [ middleware ]
      [ Dream.get "/:replay_id" (fun request -> assert false) ]
  ;;
end
