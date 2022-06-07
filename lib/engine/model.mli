type t =
  { timeout : float option
  ; scenes : Scenes.t
  }
[@@deriving sexp, equal]

val init : g:float -> t
val of_scenes : Scenes.t -> time:float -> scene:Scene.t -> timeout:float option -> t

module Diff : sig
  type tt = t

  type t =
    { init : [ `Init of Scene.t | `Since of float ]
    ; scene_diffs : Scene.Diff.t list
    ; new_timeout : float option
    }

  include Common.Utils.Diff with type tt := tt and type t := t
end
