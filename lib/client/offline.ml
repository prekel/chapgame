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
      let route : Location.t = [ "offline" ], []

      let component =
        let%sub state, dispatch =
          Bonsai.state_machine0
            [%here]
            (module S.Model)
            (module struct
              type t =
                [ `Action of S.Action.t
                | `Replace of S.Model.t
                ]
              [@@deriving sexp, equal]
            end)
            ~default_model:(R.replay "start")
            ~apply_action:(fun ~inject:_ ~schedule_event:_ model action ->
              match action with
              | `Replace _ as action -> S.Engine.update model ~action
              | `Action _ as action ->
                let _, diff = S.Engine.recv_with_diff model ~action in
                S.Engine.update model ~action:(`Diff diff))
        in
        SC.scene
          ~state
          ~dispatch
          ~time_changed_manually:
            (Bonsai.Value.return (fun t ->
                 printf "%f\n" t;
                 Effect.Ignore))
      ;;
    end
