open Core
open Lwt.Let_syntax
module P = Chapgame.Protocol.Make ((val Chapgame.Utils.make_consts ~eps:1e-6))
module S = P.S

module Client = struct
  module Id = Chapgame.Utils.MakeIntId (struct
    let module_name = "Client.Id"
  end)

  type t = { websocket : Dream.websocket }
end

module Clients = struct
  type t = (Client.Id.t, Client.t) Hashtbl.t
end

module Room = struct
  module Id = Chapgame.Utils.MakeIntId (struct
    let module_name = "Room.Id"
  end)

  type t =
    { model : S.Model.t
    ; clients : Clients.t
    }

  let init () =
    { model = S.Model.init ~g:10.; clients = Hashtbl.create (module Client.Id) }
  ;;

  let init1 () =
    { (init ()) with
      model =
        S.Model.init ~g:10.
        |> S.Engine.recv
             ~action:
               { time = 0.
               ; action =
                   S.Action.AddBody
                     { id = S.Figure2.Id.next ()
                     ; x0 = 425.
                     ; y0 = 275.
                     ; r = 2.
                     ; mu = 5.
                     ; m = Float.(pi * 2. * 2.)
                     }
               ; timeout = None
               }
        |> S.Engine.recv
             ~action:
               { time = 0.
               ; action =
                   S.Action.AddBody
                     { id = S.Figure2.Id.next ()
                     ; x0 = 450.
                     ; y0 = 250.
                     ; r = 10.
                     ; mu = 1.
                     ; m = Float.(pi * 10. * 10.)
                     }
               ; timeout = None
               }
        |> S.Engine.recv
             ~action:
               { time = 0.
               ; action =
                   S.Action.AddBody
                     { id = S.Figure2.Id.next ()
                     ; x0 = 600.
                     ; y0 = 600.
                     ; r = 50.
                     ; mu = 2.
                     ; m = Float.(pi * 50. * 50.)
                     }
               ; timeout = None
               }
        |> S.Engine.recv
             ~action:
               { time = 0.
               ; action =
                   S.Action.AddBody
                     { id = S.Figure2.Id.next ()
                     ; x0 = 500.
                     ; y0 = 500.
                     ; r = 60.
                     ; mu = 3.
                     ; m = Float.(pi * 60. * 60.)
                     }
               ; timeout = None
               }
        |> S.Engine.recv
             ~action:
               { time = 0.
               ; action = S.Action.AddPoint { x = 400.; y = 200. }
               ; timeout = None
               }
        |> S.Engine.recv
             ~action:
               { time = 0.
               ; action = S.Action.AddPoint { x = 1100.; y = 100. }
               ; timeout = None
               }
        |> S.Engine.recv
             ~action:
               { time = 0.
               ; action = S.Action.AddPoint { x = 100.; y = 700. }
               ; timeout = None
               }
        |> S.Engine.recv
             ~action:
               { time = 0.
               ; action = S.Action.AddPoint { x = 700.; y = 700. }
               ; timeout = None
               }
        |> S.Engine.recv
             ~action:
               { time = 0.
               ; action = S.Action.AddPoint { x = 650.; y = 325. }
               ; timeout = None
               }
        |> S.Engine.recv
             ~action:
               { time = 0.
               ; action = S.Action.AddPoint { x = 600.; y = 400. }
               ; timeout = None
               }
        |> S.Engine.recv
             ~action:
               { time = 0.
               ; action = S.Action.AddPoint { x = 700.; y = 450. }
               ; timeout = None
               }
        |> S.Engine.recv
             ~action:
               { time = 0.
               ; action =
                   S.Action.AddLine
                     (S.LineSegmentRay.of_points
                        ~p1:{ x = 650.; y = 325. }
                        ~p2:{ x = 600.; y = 400. }
                        ~kind:`Segment)
               ; timeout = None
               }
        |> S.Engine.recv
             ~action:
               { time = 0.
               ; action =
                   S.Action.AddLine
                     (S.LineSegmentRay.of_points
                        ~p1:{ x = 600.; y = 400. }
                        ~p2:{ x = 700.; y = 450. }
                        ~kind:`Segment)
               ; timeout = None
               }
        |> S.Engine.recv
             ~action:
               { time = 0.
               ; action =
                   S.Action.AddLine
                     (S.LineSegmentRay.of_points
                        ~p1:{ x = 400.; y = 200. }
                        ~p2:{ x = 1100.; y = 100. }
                        ~kind:`Segment)
               ; timeout = None
               }
        |> S.Engine.recv
             ~action:
               { time = 0.
               ; action =
                   S.Action.AddLine
                     (S.LineSegmentRay.of_points
                        ~p1:{ x = 1100.; y = 100. }
                        ~p2:{ x = 700.; y = 700. }
                        ~kind:`Line)
               ; timeout = None
               }
        |> S.Engine.recv
             ~action:
               { time = 0.
               ; action =
                   S.Action.AddLine
                     (S.LineSegmentRay.of_points
                        ~p1:{ x = 700.; y = 700. }
                        ~p2:{ x = 100.; y = 700. }
                        ~kind:`Ray)
               ; timeout = None
               }
        |> S.Engine.recv
             ~action:
               { time = 0.
               ; action =
                   S.Action.AddLine
                     (S.LineSegmentRay.of_points
                        ~p1:{ x = 100.; y = 700. }
                        ~p2:{ x = 400.; y = 200. }
                        ~kind:`Segment)
               ; timeout = None
               }
    }
  ;;
end

let model_pi m2 =
  let id1 = S.Figure2.Id.next () in
  let id2 = S.Figure2.Id.next () in
  S.Model.init ~g:10.
  |> S.Engine.recv
       ~action:
         { time = 0.
         ; action = AddBody { id = id1; x0 = 5.; y0 = 2.; r = 1.; mu = 0.; m = 1. }
         ; timeout = Some 0.
         }
  |> S.Engine.recv
       ~action:
         { time = 0.
         ; action = AddBody { id = id2; x0 = 10.; y0 = 2.; r = 1.; mu = 0.; m = m2 }
         ; timeout = Some 0.
         }
  |> S.Engine.recv
       ~action:{ time = 0.; action = AddPoint { x = 0.; y = 2. }; timeout = Some 0. }
  |> S.Engine.recv
       ~action:
         { time = 0.
         ; action = GiveVelocity { id = id1; v0 = -1., 0. }
         ; timeout = Some 0.
         }
  |> S.Engine.recv
       ~action:
         { time = 0.
         ; action = GiveVelocity { id = id2; v0 = -1., 0. }
         ; timeout = Some 0.
         }
  |> S.Engine.recv ~action:{ time = 0.; action = Empty; timeout = None }
;;

module Rooms = struct
  type t = (Room.Id.t, Room.t) Hashtbl.t

  let init () = Hashtbl.create (module Room.Id)
end

let rooms_field : Rooms.t Dream.field = Dream.new_field ()
let rooms_var = Rooms.init ()

let rooms_middleware inner_handler request =
  Dream.set_field request rooms_field rooms_var;
  inner_handler request
;;

module DreamExt = struct
  let sexp ?status ?code ?headers ?mach sexp =
    let body =
      match mach with
      | Some true -> Sexp.to_string_mach sexp
      | _ -> Sexp.to_string_hum sexp
    in
    let response = Dream.response ?status ?code ?headers body in
    Dream.set_header response "Content-Type" "text/plain; charset=utf-8";
    Lwt.return response
  ;;
end

let room_of_request request =
  let rooms = Dream.field request rooms_field |> Option.value_exn in
  let room_id = Dream.param request "room_id" |> Room.Id.of_string in
  rooms, Hashtbl.find_exn rooms room_id, room_id
;;

let update_room ~(rooms : Rooms.t) ~(room : Room.t) ~room_id action =
  match action with
  | `Action _ as action ->
    let model, diff = S.Engine.recv_with_diff room.model ~action in
    Hashtbl.update rooms room_id ~f:(function
        | Some prev -> { prev with model }
        | _ -> assert false);
    let%map _ =
      Lwt_list.iter_s
        (fun (_id, Client.{ websocket = client }) ->
          Dream.send
            client
            ([%sexp (P.Response.Diff diff : P.Response.t)] |> Sexp.to_string_hum))
        (room.clients |> Hashtbl.to_alist)
    in
    ()
  | `Replace _ as action ->
    let model = S.Engine.update room.model ~action in
    Hashtbl.update rooms room_id ~f:(function
        | Some prev -> { prev with model }
        | _ -> assert false);
    let%map _ =
      Lwt_list.iter_s
        (fun (_id, Client.{ websocket = client }) ->
          Dream.send
            client
            ([%sexp (P.Response.Replace model : P.Response.t)] |> Sexp.to_string_hum))
        (room.clients |> Hashtbl.to_alist)
    in
    ()
;;

let loader path ~content_type =
  match Assets.read path with
  | None -> Dream.empty `Not_Found
  | Some asset ->
    Dream.respond
      asset
      ~headers:
        [ ( "Content-Type"
          , match content_type with
            | `text_html -> "text/html; charset=utf-8"
            | `application_javascript -> "application/javascript; charset=utf-8" )
        ]
;;

let () =
  Dream.run ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ Dream.router
       [ Dream.scope
           "/"
           [ Dream_encoding.compress ]
           [ Dream.get "/" (fun _ -> loader "index.html" ~content_type:`text_html)
           ; Dream.get "/main.bc.js" (fun _ ->
                 loader "main.bc.js" ~content_type:`application_javascript)
           ]
       ; Dream.scope
           "/room"
           [ rooms_middleware ]
           [ Dream.post "/create" (fun request ->
                 let rooms = Dream.field request rooms_field |> Option.value_exn in
                 let id = Room.Id.next () in
                 Hashtbl.add_exn rooms ~key:id ~data:(Room.init ());
                 DreamExt.sexp [%sexp (id : Room.Id.t)])
           ; Dream.post "/create1" (fun request ->
                 let rooms = Dream.field request rooms_field |> Option.value_exn in
                 let id = Room.Id.next () in
                 Hashtbl.add_exn rooms ~key:id ~data:(Room.init1 ());
                 DreamExt.sexp [%sexp (id : Room.Id.t)])
           ; Dream.get "/create2/:m" (fun request ->
                 let m = Dream.param request "m" |> Float.of_string in
                 let rooms = Dream.field request rooms_field |> Option.value_exn in
                 let id = Room.Id.next () in
                 Hashtbl.add_exn
                   rooms
                   ~key:id
                   ~data:{ (Room.init ()) with model = model_pi (10. ** m) };
                 DreamExt.sexp [%sexp (id : Room.Id.t)])
           ; Dream.get "/:room_id" (fun request ->
                 let _, room, _ = room_of_request request in
                 DreamExt.sexp [%sexp (room.model : S.Model.t)])
             (* ; Dream.post "/:room_id/action" (fun request -> let rooms, room, room_id =
                room_of_request request in let%bind body = Dream.body request in let
                action = body |> Sexp.of_string |> [%of_sexp: S.Action.t] in let%bind diff
                = update_room action ~room ~rooms ~room_id in DreamExt.sexp [%sexp (diff :
                S.Model.Diff.t)]) *)
           ; Dream.get "/:room_id/ws" (fun request ->
                 Dream.websocket (fun client ->
                     let client_id = Client.Id.next () in
                     let rec loop () =
                       let%bind msg = Dream.receive client in
                       let rooms, room, room_id = room_of_request request in
                       match msg with
                       | Some message ->
                         Dream.log "%s" message;
                         begin
                           match message |> Sexp.of_string |> [%of_sexp: P.Request.t] with
                           | Start client_model ->
                             Hashtbl.add_exn
                               room.clients
                               ~key:client_id
                               ~data:Client.{ websocket = client };
                             let%bind () =
                               Dream.send
                                 client
                                 ([%sexp
                                    (P.Response.Diff
                                       (S.Model.Diff.diff ~old:client_model room.model)
                                      : P.Response.t)]
                                 |> Sexp.to_string_hum)
                             in
                             loop ()
                           | Action a ->
                             let%bind () =
                               update_room (`Action a) ~room ~rooms ~room_id
                             in
                             loop ()
                           | Replace m ->
                             let%bind () =
                               update_room (`Replace m) ~room ~rooms ~room_id
                             in
                             loop ()
                         end
                       | None ->
                         Hashtbl.remove room.clients client_id;
                         Dream.close_websocket client
                     in
                     loop ()))
           ]
       ]
;;
