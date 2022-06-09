open Core
open Bonsai_web
open Bonsai.Let_syntax

let route room_id token : Location.t =
  ( [ "online"; Int.to_string room_id ]
  , match token with
    | Some token -> [ "token", token ]
    | None -> [] )
;;

module Action = struct
  type t =
    [ `Action of Engine.Action.t
    | `Replace of Engine.Model.t
    | `Prolong of Engine.Action.until
    | `Diff of Engine.Model.Diff.t
    ]
  [@@deriving sexp, equal]
end

let component ~room_id ~token:_ =
  let%sub send_msg, last_msg =
    Websocket.use
      (Uri.empty |> fun uri -> Uri.with_path uri (sprintf "/api/room/%d/ws" room_id))
  in
  let%sub model, dispatch =
    Bonsai.state_machine0
      [%here]
      (module struct
        type t = Engine.Model.t option [@@deriving sexp, equal]
      end)
      (module Action)
      ~default_model:None
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model action ->
        match model, action with
        | Some model, action -> Some (Engine.update model ~action)
        | None, `Replace model -> Some model
        | None, _ -> None)
  in
  let%sub time_changed_manually =
    let%arr send_msg = send_msg in
    fun new_time ->
      Protocol.Request.SetTime new_time
      |> [%sexp_of: Protocol.Request.t]
      |> Sexp.to_string_hum
      |> send_msg
  in
  let%sub speed_changed_manually =
    let%arr send_msg = send_msg in
    fun new_speed ->
      Protocol.Request.SetSpeed new_speed
      |> [%sexp_of: Protocol.Request.t]
      |> Sexp.to_string_hum
      |> send_msg
  in
  let%sub set_time, set_speed, inner, outer =
    Scene.scene
      ~model:
        (Bonsai.Value.map model ~f:(function
            | Some model -> model
            | None -> Engine.Model.init ~g:1.))
      ~dispatch
      ~time_changed_manually
      ~speed_changed_manually
      ~init_until:Engine.Action.{ timespan = Some 10.; quantity = Some 25 }
  in
  let%sub on_new_msg =
    let%arr dispatch = dispatch
    and set_time = set_time
    and set_speed = set_speed in
    function
    | Some msg ->
      begin
        match msg |> Sexp.of_string |> [%of_sexp: Protocol.Defaults.Response.t] with
        | Full { time; speed; payload = (); diff } ->
          let%bind.Effect () = set_time time in
          let%bind.Effect () = set_speed speed in
          dispatch (diff :> Action.t)
      end
    | None -> Effect.Ignore
  in
  let%sub () =
    Bonsai.Edge.on_change
      [%here]
      (module struct
        type t = string option [@@deriving sexp, equal]
      end)
      ~callback:on_new_msg
      last_msg
  in
  let%arr inner = inner
  and outer = outer in
  inner, outer
;;
