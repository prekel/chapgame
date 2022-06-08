open Core

type kind =
  [ `Line
  | `Segment
  | `Ray
  ]
[@@deriving sexp, equal, compare]

type t =
  { p1 : Point.t
  ; p2 : Point.t
  ; kind : kind
  }
[@@deriving sexp]

include Comparable.S with type t := t

val of_points : p1:Point.t -> p2:Point.t -> kind:[ `Line | `Ray | `Segment ] -> t
val to_points : t -> Point.t * Point.t
val kind : t -> kind
val to_abc : t -> Float.t * Float.t * Float.t
