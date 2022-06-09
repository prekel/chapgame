open Core
open Lwt.Let_syntax
module S = Engine

module Make (R : sig
  val replay : string -> S.Model.t
end) =
struct
  include
    Generic_room.Make
      (R)
      (struct
        include Unit

        let empty = ()
      end)

  module Request = Protocol.Request

  let update_room (room : Room.t) = function
    | Request.SetTime time -> Lwt.return ({ room with time }, None)
    | SetSpeed speed -> Lwt.return ({ room with speed }, None)
    | Action action ->
      Lwt_mutex.with_lock room.lock (fun () ->
          let%map model, diff =
            match action.action with
            | (`Prolong _ | `Action _) as a ->
              let%map model, diff =
                Lwt_preemptive.detach
                  (fun (model, action) -> S.recv_with_diff model ~action)
                  (room.model, a)
              in
              model, Some diff
            | `Replace model -> Lwt.return (model, None)
          in
          { room with model; time = action.time; speed = action.speed }, diff)
  ;;

  let action_route =
    Dream.post "/:room_id/action" (fun request ->
        let rooms, room, room_id = room_of_request request in
        let token = Dream.query request "token" in
        match token with
        | Some token when String.(token = room.token) ->
          let%bind body = Dream.body request in
          let r = body |> Sexp.of_string |> [%of_sexp: Request.t] in
          let%bind () =
            (* Lwt.async (fun () -> *)
            let%bind new_room, diff = update_room room r in
            Hashtbl.update rooms room_id ~f:(fun _ -> new_room);
            let response = Room.to_response ?diff new_room in
            broadcast_response room.lock room.clients response
            (* ) *)
          in
          Dream.empty `Accepted
        | _ -> Dream.empty `Forbidden)
  ;;

  let route = Dream.scope "/room" [ inject_rooms ] (action_route :: generic_routes)
end
