open Core

module MakeSolver (N : sig
  type t [@@deriving sexp, compare]

  include Comparable.S with type t := t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( ** ) : t -> t -> t
  val zero : t
  val one : t
  val of_int : int -> t
  val infinity : t
  val nan : t
  val neg_infinity : t
end) =
struct
  module Interval = struct
    type t =
      | Interval of N.t * N.t
      | Empty

    let create low high =
      if Int.(N.compare low high > 0) then Empty else Interval (low, high)
    ;;

    let of_tuple (low, high) = create low high
    let infinityInterval = N.(create neg_infinity infinity)
    let negativeInfinityInterval a = N.(create neg_infinity a)
    let positiveInfinityInterval a = N.(create a infinity)

    let intervals_of_list roots =
      match roots with
      | [] -> [ infinityInterval ]
      | [ a ] -> [ negativeInfinityInterval a; positiveInfinityInterval a ]
      | a :: b :: _ ->
        let w = roots |> Common.List.windowed2_exn |> List.map ~f:of_tuple in
        (negativeInfinityInterval a :: w) @ [ positiveInfinityInterval b ]
    ;;
  end

  module Monomial : sig
    type t [@@deriving sexp, compare, equal]

    val degree : t -> int
    val coefficient : t -> N.t
    val create : degree:int -> coefficient:N.t -> t
    val derivative : t -> t option
    val calc : t -> N.t -> N.t
    val compare_by_degree : t -> t -> int
    val ( + ) : t -> t -> t
  end = struct
    type t =
      { degree : int
      ; coefficient : N.t
      }
    [@@deriving sexp, compare, equal, fields]

    let create = Fields.create

    let derivative x =
      let open N in
      match x.degree with
      | 0 -> None
      | _ ->
        Some
          { coefficient = x.coefficient * of_int x.degree; degree = Int.(x.degree - 1) }
    ;;

    let calc monomial x =
      let open N in
      monomial.coefficient * (x ** of_int monomial.degree)
    ;;

    let compare_by_degree a b = [%compare: int] a.degree b.degree

    let ( + ) a b =
      create ~coefficient:N.(coefficient a + coefficient b) ~degree:(degree a)
    ;;
  end

  module Polynomial : sig
    type t = Monomial.t list [@@deriving sexp, equal]

    val normalize : t -> t
    val of_list : Monomial.t list -> t
    val derivative : t -> t
    val calc : N.t list -> Monomial.t -> N.t
    val degree : t -> int
  end = struct
    type t = Monomial.t list [@@deriving sexp, equal]

    let normalize =
      let open Common.Fn in
      List.sort_and_group ~compare:Monomial.compare_by_degree
      >> List.map ~f:(List.reduce_exn ~f:Monomial.( + ))
    ;;

    let of_list = normalize
    let derivative = List.filter_map ~f:Monomial.derivative

    let calc poly x =
      poly |> List.map ~f:(Monomial.calc x) |> List.sum (module N) ~f:Fn.id
    ;;

    let degree = Common.Fn.(List.hd_exn >> Monomial.degree)
  end

  let test _ =
    let _ = N.( - ) in
    let _ = N.( / ) in
    let _ = N.(zero) in
    let _ = N.(one) in
    let _ = N.(nan) in
    let _ = N.( * ) in
    ()
  ;;
end
