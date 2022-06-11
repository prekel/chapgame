module Collision : sig
  type t =
    | Collision of
        { id1 : Body.Id.t
        ; id2 : Body.Id.t
        }
    | CollisionWithPoint of
        { id : Body.Id.t
        ; point : Point.t
        }
    | CollisionWithLine of
        { id : Body.Id.t
        ; line : Line.t
        }
  [@@deriving sexp, equal, compare]
end

module Cause : sig
  type t =
    [ `Action of Action.t
    | `Collision of Collision.t
    | `Init
    ]
  [@@deriving sexp, equal, compare]
end

type t =
  { time : float
  ; bodies : Bodies.t
  ; points : Open.Points.t
  ; lines : Open.Lines.t
  ; global_values : Values.t
  ; cause : Cause.t list
  }
[@@deriving sexp, equal]

val update
  :  ?bodies:Bodies.t
  -> ?points:Open.Points.t
  -> ?lines:Open.Lines.t
  -> ?cause:Cause.t list
  -> ?time:float
  -> t
  -> t

val add_body_values
  :  Bodies.t
  -> id:Body.Id.t
  -> values:Values.t
  -> rules:Rule.t list
  -> Bodies.t

val add_body
  :  Bodies.t
  -> id:Body.Id.t
  -> x0:float
  -> y0:float
  -> r:float
  -> mu:float
  -> m:float
  -> Bodies.t

val init : g:float -> t

module Diff : sig
  type tt = t

  type t =
    { new_time : float
    ; bodies_diff : Bodies.Diff.t
    ; points_diff : Open.Points.Diff.t
    ; lines_diff : Open.Lines.Diff.t
    ; global_values_diff : Values.Diff.t
    ; new_cause : Cause.t list
    }
  [@@deriving sexp, equal]

  include Common.Utils.Diff with type t := t and type tt := tt
end
