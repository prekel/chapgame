open Core

type t =
  { time : float
  ; speed : float
  ; action : [ `Action of Engine.Action.t | `Replace of Engine.Model.t ]
  }
[@@deriving sexp, equal]
