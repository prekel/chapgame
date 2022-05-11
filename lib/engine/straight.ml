open Core

module Make
    (N : Solver.Module_types.NUMBER)
    (Point : module type of Point.Make (N)) =
    struct
  module T = struct
    type t =
      { p1 : Point.t
      ; p2 : Point.t
      ; kind : [ `Line | `Segment | `Ray ]
      }
    [@@deriving sexp, equal, compare]
  end

  include T
  include Comparable.Make (T)

  let of_points ~p1 ~p2 ~kind = { p1; p2; kind }

  let to_abc { p1; p2; kind = _ } =
    N.(p2.y - p1.y, p1.x - p2.x, (p1.y * p2.x) - (p1.x * p2.y))
  ;;
end
