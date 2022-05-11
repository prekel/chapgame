open Core

module Make
    (C : Engine.Module_types.CONSTS)
    (S : module type of Engine.Scene.Make (C)) (Payload : sig
      include Sexpable.S
      include Equal.S with type t := t
    end) =
    struct
  module Protocol = Engine.Protocol.Make1 (C) (S) (Payload)

  module Client = struct
    module Id = Common.Utils.MakeIntId (struct
      let module_name = "Client.Id"
    end)

    type t = { websocket : Dream.websocket }
  end

  module Clients = struct
    type t = (Client.Id.t, Client.t) Hashtbl.t
  end

  module Room = struct
    module Id = Common.Utils.MakeIntId (struct
      let module_name = "Room.Id"
    end)

    type t =
      { model : S.Model.t
      ; clients : Clients.t
      ; lock : Lwt_mutex.t
      }
  end

  module Rooms = struct
    type t = (Room.Id.t, Room.t) Hashtbl.t

    let init () = Hashtbl.create (module Room.Id)
  end

  let route =
    Dream.scope
      "/room"
      []
      [ Dream.post "/:room_id/action" (fun _request -> Dream.empty `OK)
      ; Dream.get "/:room_id/ws" (fun request ->
            Dream.websocket (fun client -> assert false))
      ]
  ;;
end