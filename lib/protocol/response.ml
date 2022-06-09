open Core

module Make (Payload : sig
  type t [@@deriving sexp, equal]
end) =
struct
  type f =
    { time : float
    ; speed : float
    ; payload : Payload.t
    ; diff : [ `Diff of Engine.Model.Diff.t | `Replace of Engine.Model.t ]
    }
  [@@deriving sexp, equal]

  type t =
    | Full of f
    | TimeChanged of float
    | SpeedChanged of float
  [@@deriving sexp, equal]
end
