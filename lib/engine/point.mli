open Core

module Make (N : Solver.Module_types.NUMBER) : sig
  type t =
    { x : N.t
    ; y : N.t
    }

  include Sexpable.S with type t := t
  include Comparable.S with type t := t

end
