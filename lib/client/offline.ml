open Bonsai_web
open Bonsai.Let_syntax
module SC = Scene

module Make (R : sig
  val replay : string -> Engine.Model.t
end) =
struct
  let route : Location.t = [ "" ], []

  let component =
    let%sub model, dispatch =
      Bonsai.state_machine0
        [%here]
        (module Engine.Model)
        (module struct
          type t =
            [ `Action of Engine.Action.t
            | `Replace of Engine.Model.t
            | `Prolong of Engine.Action.until
            ]
          [@@deriving sexp, equal]
        end)
        ~default_model:(R.replay "start")
        ~apply_action:(fun ~inject:_ ~schedule_event:_ model action ->
          Engine.update model ~action)
    in
    let%sub scene =
      SC.scene
        ~model
        ~dispatch
        ~time_changed_manually:(Bonsai.Value.return (fun _ -> Effect.Ignore))
        ~speed_changed_manually:(Bonsai.Value.return (fun _ -> Effect.Ignore))
        ~init_until:Engine.Action.{ timespan = Some 10.; quantity = Some 25 }
    in
    let%arr _, _, inner, outer = scene in
    inner, outer
  ;;
end
