open Core

module type S = sig
  module N : Solver.Module_types.NUMBER

  type t =
    { x : N.t
    ; y : N.t
    }

  include Sexpable.S with type t := t
  include Comparable.S with type t := t
end

module type Intf = sig
  module type S = S

  module Make (N : Solver.Module_types.NUMBER) : S with module N = N
end
