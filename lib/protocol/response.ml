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
    | Chunk of Engine.Model.Diff.t
  [@@deriving sexp, equal]
end
