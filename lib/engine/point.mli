open Core

type t =
  { x : float
  ; y : float
  }

include Sexpable.S with type t := t
include Comparable.S with type t := t
