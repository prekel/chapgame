open Core

type t =
  | SetTime of float
  | SetSpeed of float
  | Action of
      { time : float
      ; speed : float
      ; action : [ `Action of Engine.Action.t | `Replace of Engine.Model.t ]
      }
[@@deriving sexp, equal]
