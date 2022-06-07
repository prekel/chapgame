open Open

type t =
  { timeout : N.t option
  ; scenes : Scenes.t
  }
[@@deriving sexp, equal]

val init : g:N.t -> t
val of_scenes : Scenes.t -> time:N.t -> scene:Scene.t -> timeout:N.t option -> t

module Diff : sig
  type tt = t

  type t =
    { init : [ `Init of Scene.t | `Since of N.t ]
    ; scene_diffs : Scene.Diff.t list
    ; new_timeout : N.t option
    }

  include Common.Utils.Diff with type tt := tt and type t := t
end
