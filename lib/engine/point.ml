open Core

module Make (N : Solver.Module_types.NUMBER) = struct
  module T = struct
    type t =
      { x : N.t
      ; y : N.t
      }
    [@@deriving sexp, equal, compare]
  end

  include T
  include Comparable.Make (T)
end
