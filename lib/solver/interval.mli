module Make (N :Module_types.NUMBER) : sig
  type t =
    | Interval of
        { left : N.t
        ; right : N.t
        }
    | NegInfinity of { right : N.t }
    | PosInfinity of { left : N.t }
    | Infinity
    | Empty
  [@@deriving sexp, equal]

  val create : left:N.t -> right:N.t -> t
  val of_tuple : N.t * N.t -> t
  val to_tuple : t -> (N.t * N.t) option
  val infinity : t
  val neg_infinity : right:N.t -> t
  val pos_infinity : left:N.t -> t
  val empty : t
  val intervals_of_list : N.t list -> t list
  val difference : t -> N.t
  val left_trunc : t -> N.t
  val right_trunc : t -> N.t
end
