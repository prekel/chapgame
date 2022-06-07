(* open Core
open Lwt.Let_syntax

let random = Stdlib.Random.State.make_self_init ()

module Make
    (C : Engine.Module_types.CONSTS)
    (S : module type of Engine.Scene.Make (C)) (R : sig
      val replay : string -> S.Model.t
    end) (Payload : sig
      type t [@@deriving sexp, equal]

      val empty : t
    end) =
    struct
  module N = C.N
  module Response = Protocol.Response.Make (C) (S) (Payload)

  module Client = struct
    module Id = Common.Utils.MakeIntId (struct
      let module_name = "Client.Id"
    end)

    type t = { websocket : Dream.websocket }
  end

  module Clients = struct
    type t = (Client.Id.t, Client.t) Hashtbl.t

    let create () = Hashtbl.create (module Client.Id)
  end

  module Room = struct
    module Id = Common.Utils.MakeIntId (struct
      let module_name = "Room.Id"
    end)

    type t =
      { model : S.Model.t
      ; clients : (Clients.t[@sexp.opaque])
      ; lock : (Lwt_mutex.t[@sexp.opaque])
      ; time : N.t
      ; speed : N.t
      ; payload : Payload.t
      ; token : (string[@sexp.opaque])
      }
    [@@deriving sexp]

    let to_response ?diff (room : t) =
      Response.(
        Full
          { time = room.time
          ; speed = room.speed
          ; payload = room.payload
          ; diff =
              (match diff with
              | Some diff -> `Diff diff
              | None -> `Replace room.model)
          })
    ;;

    let init ~token ~payload model =
      { model
      ; clients = Clients.create ()
      ; lock = Lwt_mutex.create ()
      ; time = N.zero
      ; speed = N.one
      ; payload
      ; token =
          (match token with
          | Some token -> token
          | None -> Uuidm.v4_gen random () |> Uuidm.to_string)
      }
    ;;
  end

  module Rooms = struct
    type t = (Room.Id.t, Room.t) Hashtbl.t

    let init () = Hashtbl.create (module Room.Id)
  end

  let rooms_field : Rooms.t Dream.field = Dream.new_field ()
  let rooms_var = Rooms.init ()

  let inject_rooms inner_handler request =
    Dream.set_field request rooms_field rooms_var;
    inner_handler request
  ;;

  let room_of_request request =
    let rooms = Dream.field request rooms_field |> Option.value_exn in
    let room_id = Dream.param request "room_id" |> Room.Id.of_string in
    rooms, Hashtbl.find_exn rooms room_id, room_id
  ;;

  let broadcast_response lock clients response =
    Lwt_mutex.with_lock lock (fun () ->
        let clients = Hashtbl.to_alist clients in
        let message = response |> [%sexp_of: Response.t] |> Sexp.to_string_hum in
        Lwt_list.iter_p
          (fun (_id, Client.{ websocket }) -> Dream.send websocket message)
          clients)
  ;;

  let generic_routes =
    [ Dream.post "/create" (fun request ->
          let rooms = Dream.field request rooms_field |> Option.value_exn in
          let token = Dream.query request "token" in
          match Dream.query request "replay_id" with
          | Some replay_id ->
            let id = Room.Id.next () in
            Hashtbl.add_exn
              rooms
              ~key:id
              ~data:(Room.init ~token ~payload:Payload.empty (R.replay replay_id));
            Dream_ext.sexp [%sexp (id : Room.Id.t)]
          | None ->
            let%bind body = Dream.body request in
            let id = Room.Id.next () in
            Hashtbl.add_exn
              rooms
              ~key:id
              ~data:
                (Room.init
                   ~token
                   ~payload:Payload.empty
                   (body |> Sexp.of_string |> [%of_sexp: S.Model.t]));
            Dream_ext.sexp [%sexp (id : Room.Id.t)])
    ; Dream.get "/:room_id" (fun request ->
          let _, room, _ = room_of_request request in
          Dream_ext.sexp [%sexp (room : Room.t)])
    ; Dream.get "/:room_id/model" (fun request ->
          let _, room, _ = room_of_request request in
          Dream_ext.sexp [%sexp (room.model : S.Model.t)])
    ; Dream.get "/:room_id/ws" (fun request ->
          Dream.websocket (fun client ->
              let client_id = Client.Id.next () in
              let _, room, _ = room_of_request request in
              let%bind () =
                Room.to_response room
                |> [%sexp_of: Response.t]
                |> Sexp.to_string_hum
                |> Dream.send client
              in
              let rec loop () =
                let%bind msg = Dream.receive client in
                match msg with
                | Some _ -> loop ()
                | None ->
                  Lwt_mutex.with_lock room.lock (fun () ->
                      Hashtbl.remove room.clients client_id;
                      Dream.close_websocket client)
              in
              loop ()))
    ]
  ;;
end *)
