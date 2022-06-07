open Core

module T = struct
  type t =
    { x : float
    ; y : float
    }
  [@@deriving sexp, equal, compare]
end

include T
include Comparable.Make (T)
