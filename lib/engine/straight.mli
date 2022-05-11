open Core

module Make (N : Solver.Module_types.NUMBER) (Point : module type of Point.Make (N)) : sig
  type t =
    { p1 : Point.t
    ; p2 : Point.t
    ; kind : [ `Line | `Segment | `Ray ]
    }

  include Sexpable.S with type t := t
  include Comparable.S with type t := t

  val of_points : p1:Point.t -> p2:Point.t -> kind:[ `Line | `Ray | `Segment ] -> t
  val to_abc : t -> N.t * N.t * N.t
end
