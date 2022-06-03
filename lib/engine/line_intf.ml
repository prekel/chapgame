open Core

module type S = sig
  module N : Solver.Module_types.NUMBER
  module Point : Point.S with module N = N

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

module type Intf = sig
  module type S = S

  module Make (N : Solver.Module_types.NUMBER) (Point : Point.S with module N = N) :
    S with module N = N and module Point = Point
end
