open Core
open Open

module Collision = struct
  type t =
    | Collision of
        { id1 : Body.Id.t
        ; id2 : Body.Id.t
        }
    | CollisionWithPoint of
        { id : Body.Id.t
        ; point : Point.t
        }
    | CollisionWithLine of
        { id : Body.Id.t
        ; line : Line.t
        }
  [@@deriving sexp, equal, compare]
end

module Cause = struct
  type t =
    [ `Init
    | `Action of Action.t
    | `Collision of Collision.t
    ]
  [@@deriving sexp, equal, compare]
end

type t =
  { time : N.t
  ; bodies : Bodies.t
  ; points : Points.t
  ; lines : Lines.t
  ; global_values : Values.t
  ; cause : Cause.t list
  }
[@@deriving sexp, equal]

let update ?bodies ?points ?lines ?cause ?time s =
  { s with
    time = Option.value time ~default:s.time
  ; bodies = Option.value bodies ~default:s.bodies
  ; points = Option.value points ~default:s.points
  ; lines = Option.value lines ~default:s.lines
  ; cause = Option.value cause ~default:s.cause
  }
;;

let add_body_values bodies ~id ~values ~rules =
  Bodies.add bodies ~id ~body:Body.{ id; values; rules }
;;

let add_body bodies ~id ~x0 ~y0 ~r ~mu ~m =
  add_body_values
    bodies
    ~id
    ~values:
      (Values.of_alist
         [ `x0, x0; `y0, y0; `v0_x, N.zero; `v0_y, N.zero; `r, r; `mu, mu; `m, m ])
    ~rules:Rule.rules0
;;

let init ~g =
  { time = N.zero
  ; bodies = Bodies.empty
  ; points = Points.empty
  ; lines = Lines.empty
  ; global_values = Values.of_alist [ `g, g ]
  ; cause = [ `Init ]
  }
;;

module Diff = struct
  type t =
    { new_time : N.t
    ; bodies_diff : Bodies.Diff.t
    ; points_diff : Points.Diff.t
    ; lines_diff : Lines.Diff.t
    ; global_values_diff : Values.Diff.t
    ; new_cause : Cause.t list
    }
  [@@deriving sexp, equal]

  let diff ~old curr =
    let new_time = curr.time in
    let bodies_diff = Bodies.Diff.diff ~old:old.bodies curr.bodies in
    let points_diff = Points.Diff.diff ~old:old.points curr.points in
    let lines_diff = Lines.Diff.diff ~old:old.lines curr.lines in
    let global_values_diff = Values.Diff.diff ~old:old.global_values curr.global_values in
    let new_cause = curr.cause in
    { new_time; bodies_diff; points_diff; lines_diff; global_values_diff; new_cause }
  ;;

  let apply_diff ~diff old =
    { time = diff.new_time
    ; bodies = Bodies.Diff.apply_diff ~diff:diff.bodies_diff old.bodies
    ; points = Points.Diff.apply_diff ~diff:diff.points_diff old.points
    ; lines = Lines.Diff.apply_diff ~diff:diff.lines_diff old.lines
    ; global_values =
        Values.Diff.apply_diff ~diff:diff.global_values_diff old.global_values
    ; cause = diff.new_cause
    }
  ;;
end
