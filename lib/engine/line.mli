open Core

module Make (N : Solver.Module_types.NUMBER) (Point : module type of Point.Make (N)) : sig
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
  val to_abc : t -> N.t * N.t * N.t
end