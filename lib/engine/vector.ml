module Make (N : Solver.Module_types.NUMBER) = struct
  type t = N.t * N.t [@@deriving equal]

  let zero = N.zero, N.zero

  open N

  let ( + ) (x1, y1) (x2, y2) = x1 + x2, y1 + y2
  let ( - ) (x1, y1) (x2, y2) = x1 - x2, y1 - y2
  let ( * ) (x1, y1) (x2, y2) = x1 * x2, y1 * y2
  let ( / ) (x1, y1) (x2, y2) = x1 / x2, y1 / y2
  let ( ~- ) (x, y) = -x, -y

  let dot a b =
    let x, y = a * b in
    N.(x + y)
  ;;

  let len_sqr (x, y) = N.((x * x) + (y * y))
  let len a = N.(sqrt (len_sqr a))
  let ( *^ ) (a, b) c = N.(a * c, b * c)
  let ( ^* ) c (a, b) = N.(a * c, b * c)
end