open Core

module MakeSolver (N : sig
  type t [@@deriving sexp, compare, equal]

  val ( ~- ) : t -> t
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
  module Interval : sig
    type t [@@deriving sexp, equal]

    val case
      :  interval:(N.t -> N.t -> 'a)
      -> neg_infinity:(N.t -> 'a)
      -> pos_infinity:(N.t -> 'a)
      -> infinity:'a
      -> empty:'a
      -> t
      -> 'a

    val create : N.t -> N.t -> t
    val of_tuple : N.t * N.t -> t
    val infinity : t
    val neg_infinity : N.t -> t
    val pos_infinity : N.t -> t
    val empty : t
    val intervals_of_list : N.t list -> t list
  end = struct
    type t =
      | Interval of N.t * N.t
      | Neg_Infinity of N.t
      | Pos_Infinity of N.t
      | Infinity
      | Empty
    [@@deriving sexp, equal, variants]

    let case ~interval ~neg_infinity ~pos_infinity ~infinity ~empty = function
      | Interval (a, b) -> interval a b
      | Neg_Infinity a -> neg_infinity a
      | Pos_Infinity a -> pos_infinity a
      | Infinity -> infinity
      | Empty -> empty
    ;;

    let create low high =
      if N.(equal low neg_infinity && equal high infinity)
      then Infinity
      else if N.(equal low neg_infinity)
      then Neg_Infinity high
      else if N.(equal high infinity)
      then Pos_Infinity low
      else if Int.(N.compare low high > 0)
      then Empty
      else Interval (low, high)
    ;;

    let of_tuple (low, high) = create low high

    let intervals_of_list roots =
      match roots with
      | [] -> [ infinity ]
      | [ a ] -> [ neg_infinity a; pos_infinity a ]
      | a :: b :: _ ->
        let w = roots |> Common.List.windowed2_exn |> List.map ~f:of_tuple in
        (neg_infinity a :: w) @ [ pos_infinity b ]
    ;;
  end

  module Monomial : sig
    type t [@@deriving sexp, compare, equal]

    val degree : t -> int
    val coefficient : t -> N.t
    val create : coefficient:N.t -> degree:int -> t
    val derivative : t -> t option
    val calc : t -> x:N.t -> N.t
    val compare_by_degree : t -> t -> int
    val ( + ) : t -> t -> t
  end = struct
    type t =
      { coefficient : N.t
      ; degree : int
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

    let calc mono ~x =
      let open N in
      mono.coefficient * (x ** of_int mono.degree)
    ;;

    let compare_by_degree a b = [%compare: int] a.degree b.degree

    let ( + ) a b =
      create ~coefficient:N.(coefficient a + coefficient b) ~degree:(degree a)
    ;;
  end

  module LinearEquation : sig
    val root : a:N.t -> b:N.t -> N.t
  end = struct
    let root ~a ~b = N.(-b / a)
  end

  module Polynomial : sig
    type t [@@deriving sexp, equal]

    val normalize : t -> t
    val of_list : Monomial.t list -> t
    val derivative : t -> t
    val calc : t -> x:N.t -> N.t
    val degree : t -> int
    val linear_root : t -> N.t option
  end = struct
    type t = Monomial.t list [@@deriving sexp, equal]

    let normalize =
      let open Common.Fn in
      List.sort_and_group ~compare:Monomial.compare_by_degree
      >> List.map ~f:(List.reduce_exn ~f:Monomial.( + ))
    ;;

    let of_list = normalize
    let derivative = List.filter_map ~f:Monomial.derivative

    let calc poly ~x =
      poly |> List.map ~f:(Monomial.calc ~x) |> List.sum (module N) ~f:Fn.id
    ;;

    let degree = Common.Fn.(List.hd_exn >> Monomial.degree)

    let linear_root =
      let open Monomial in
      function
      | [ x0; x1 ] when degree x0 = 0 && degree x1 = 1 ->
        let b = coefficient x0 in
        let a = coefficient x1 in
        Some (LinearEquation.root ~a ~b)
      | [ x1 ] when degree x1 = 1 -> Some N.zero
      | _ -> None
    ;;
  end

  module BinarySearch = struct
    let median =
      Interval.case
        ~infinity:N.zero
        ~neg_infinity:(fun right -> N.(right - one))
        ~pos_infinity:(fun left -> N.(left + one))
        ~interval:(fun l r -> N.((l + r) / (one + one)))
        ~empty:N.zero
    ;;

    let ends_difference ~f global_median =
      Interval.case
        ~infinity:N.(f one - f (-one))
        ~neg_infinity:(fun right -> N.(f right - f global_median))
        ~pos_infinity:(fun left -> N.(f global_median - f left))
        ~interval:(fun l r -> N.(f r - f l))
        ~empty:N.zero
    ;;
  end

  module PolynomialEquation : sig
    val roots : eps:N.t -> Polynomial.t -> N.t list
  end = struct
    let filter_intervals
        : eps:N.t -> calc:(x:N.t -> N.t) -> Interval.t list -> Interval.t list
      =
      assert false
    ;;

    let rec roots ~eps poly =
      match Polynomial.degree poly with
      | 1 -> poly |> Polynomial.linear_root |> Option.to_list
      | _ ->
        let calc = Polynomial.calc poly in
        poly
        |> Polynomial.derivative
        |> roots ~eps
        |> Interval.intervals_of_list
        |> filter_intervals ~eps ~calc
        |> List.map ~f:(assert false)
        |> List.sort ~compare:N.compare
    ;;
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
