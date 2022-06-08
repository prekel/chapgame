open Core

module T = struct
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
  [@@deriving sexp, equal, compare]
end

include T
include Comparable.Make (T)

let of_points ~p1 ~p2 ~kind = { p1; p2; kind }
let to_points { p1; p2; _ } = p1, p2
let kind { kind; _ } = kind

let to_abc { p1; p2; kind = _ } =
  Float.(p2.y - p1.y, p1.x - p2.x, (p1.y * p2.x) - (p1.x * p2.y))
;;
