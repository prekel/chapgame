open Core
open Open

type t =
  { timeout : N.t option
  ; scenes : Scenes.t
  }
[@@deriving sexp, equal]

let init ~g =
  { scenes = Map.of_alist_exn (module N) [ N.zero, Scene.init ~g ] |> Scenes.of_map
  ; timeout = None
  }
;;

let of_scenes scenes ~time ~scene ~timeout =
  { scenes =
      Map.update (Scenes.to_map scenes) time ~f:(function
          | Some s ->
            Scene.
              { bodies = scene.bodies
              ; cause = scene.cause @ s.Scene.cause
              ; points = scene.points
              ; lines = scene.lines
              ; global_values = scene.global_values
              ; time
              }
          | None -> scene)
      |> Scenes.of_map
  ; timeout
  }
;;

module Diff = struct
  type tt = t

  type t =
    { init : [ `Init of Scene.t | `Since of N.t ]
    ; scene_diffs : Scene.Diff.t list
    ; new_timeout : N.t option
    }
  [@@deriving sexp, equal]

  let diff ~old curr =
    let old_keys = Map.keys (Scenes.to_map old.scenes) |> Sequence.of_list in
    let new_keys = Map.keys (Scenes.to_map curr.scenes) |> Sequence.of_list in
    let zipped = Sequence.zip_full old_keys new_keys in
    let init =
      Sequence.fold_until
        zipped
        ~init:None
        ~f:
          (fun acc -> function
            | `Both (a, b) when N.(a = b) ->
              let av = Scenes.get_by_id ~id:a old.scenes in
              let bv = Scenes.get_by_id ~id:b curr.scenes in
              if Scene.equal av bv then Continue (Some a) else Stop acc
            | _ -> Stop acc)
        ~finish:(fun acc -> acc)
      |> Option.map ~f:(fun since -> `Since since)
      |> Option.value
           ~default:(`Init (curr.scenes |> Scenes.to_map |> Map.min_elt_exn |> snd))
    in
    let init_time, init_scene =
      match init with
      | `Init scene -> scene.time, scene
      | `Since since -> since, Map.find_exn (Scenes.to_map curr.scenes) since
    in
    let scene_diffs =
      Scenes.to_map curr.scenes
      |> Map.filter_keys ~f:N.(fun k -> k > init_time)
      |> Map.to_sequence
      |> Sequence.folding_map ~init:init_scene ~f:(fun prev (_, curr) ->
             curr, Scene.Diff.diff ~old:prev curr)
      |> Sequence.to_list
    in
    { init; scene_diffs; new_timeout = curr.timeout }
  ;;

  let apply_diff ~diff old =
    let common_scenes, init_scene =
      match diff.init with
      | `Init init -> Map.singleton (module N) init.time init, init
      | `Since since ->
        let l, r = Scenes.before old.scenes ~time:since in
        Scenes.to_map l, r
    in
    let scenes =
      diff.scene_diffs
      |> Sequence.of_list
      |> Sequence.folding_map ~init:init_scene ~f:(fun prev diff ->
             let ret = Scene.Diff.apply_diff ~diff prev in
             ret, (ret.time, ret))
      |> Map.of_sequence_exn (module N)
      |> Map.merge_skewed common_scenes ~combine:(fun ~key:_ l _ -> l)
      |> Scenes.of_map
    in
    { scenes; timeout = diff.new_timeout }
  ;;
end
