open Core

module Make (N :Module_types.NUMBER) = struct
  type t =
    | Interval of
        { left : N.t
        ; right : N.t
        }
    | NegInfinity of { right : N.t }
    | PosInfinity of { left : N.t }
    | Infinity
    | Empty
  [@@deriving sexp, equal, variants]

  let neg_infinity = neginfinity
  let pos_infinity = posinfinity

  let create ~left:low ~right:high =
    if N.(equal low neg_infinity && equal high infinity)
    then Infinity
    else if N.(equal low neg_infinity)
    then NegInfinity { right = high }
    else if N.(equal high infinity)
    then PosInfinity { left = low }
    else if Int.(N.compare low high > 0)
    then Empty
    else Interval { left = low; right = high }
  ;;

  let of_tuple (left, right) = create ~left ~right

  let to_tuple = function
    | Interval { left; right } -> Some (left, right)
    | NegInfinity { right } -> Some N.(neg_infinity, right)
    | PosInfinity { left } -> Some N.(left, infinity)
    | Infinity -> Some N.(neg_infinity, infinity)
    | Empty -> None
  ;;

  let intervals_of_list = function
    | [] -> [ infinity ]
    | [ a ] -> [ neg_infinity ~right:a; pos_infinity ~left:a ]
    | a :: _ as roots ->
      (neg_infinity ~right:a :: (List.windowed2 roots |> List.map ~f:of_tuple))
      @ [ pos_infinity ~left:(List.last_exn roots) ]
  ;;

  let difference = function
    | Interval { left; right } -> N.(abs (right - left))
    | NegInfinity _ | PosInfinity _ | Infinity -> N.infinity
    | Empty -> N.zero
  ;;

  let left_trunc = function
    | Interval { left; right = _ } | PosInfinity { left } -> left
    | NegInfinity _ | Infinity -> N.neg_infinity
    | Empty -> N.nan
  ;;

  let right_trunc = function
    | Interval { left = _; right } | NegInfinity { right } -> right
    | Infinity | PosInfinity _ -> N.infinity
    | Empty -> N.nan
  ;;
end
