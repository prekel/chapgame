open Core
open Bonsai_web
open Bonsai.Let_syntax

module Make
    (C : Engine.Module_types.CONSTS with module N = Float)
    (S : module type of Engine.Scene.Make (C))
    (SC : module type of Scene.Make (C) (S)) (R : sig
      val replay : string -> S.Model.t
    end) =
    struct
      let route : Location.t = [ "" ], []

      let component =
        let%sub model, dispatch =
          Bonsai.state_machine0
            [%here]
            (module S.Model)
            (module struct
              type t =
                [ `Action of S.Action.t
                | `Replace of S.Model.t
                | `Prolong of S.Action.until
                ]
              [@@deriving sexp, equal]
            end)
            ~default_model:(R.replay "start")
            ~apply_action:(fun ~inject:_ ~schedule_event:_ model action ->
              S.Engine.update model ~action
              (* match action with (* | `Action _ as action -> let _, diff =
                 S.Engine.recv_with_diff model ~action in S.Engine.update model
                 ~action:(`Diff diff) *) | `Action _ as action -> S.Engine.update model
                 ~action | `Replace _ as action -> S.Engine.update model ~action |
                 `Prolong _ as action -> S.Engine.update model ~action*))
        in
        SC.scene
          ~model
          ~dispatch
          ~time_changed_manually:
            (Bonsai.Value.return (fun t ->
                 printf "%f\n" t;
                 Effect.Ignore))
          ~speed_changed_manually:
            (Bonsai.Value.return (fun s ->
                 printf "%f\n" s;
                 Effect.Ignore))
          ~init_until:S.Action.{ timespan = Some 10.; quantity = Some 25 }
      ;;
    end
