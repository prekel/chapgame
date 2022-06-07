open Core
include Common.Utils.MakeAdvancedMap (Float) (Scene)

let before scenes ~time =
  match Map.split scenes time with
  | l, Some (k, v), _ -> Map.add_exn l ~key:k ~data:v, v
  | l, None, _ -> l, snd @@ Map.max_elt_exn l
;;

let merge_with_list scenes l =
  (* TODO *)
  let l =
    Map.of_alist_reduce
      (module Float)
      (List.map l ~f:(fun scene -> scene.Scene.time, scene))
      ~f:(fun _a b -> b)
  in
  Map.merge_skewed scenes l ~combine:(fun ~key v1 v2 ->
      Scene.update
        v2
        ~bodies:v2.bodies
        ~cause:
          (* TODO *)
          (v2.cause @ v1.cause |> List.dedup_and_sort ~compare:[%compare: Scene.Cause.t])
        ~points:v2.points
        ~lines:v2.lines
        ~time:key)
;;

let last_exn = Map.max_elt_exn >> snd
