open Core

module MakeSolver (N : sig
  type t [@@deriving sexp, compare, equal]

  include Comparable.S with type t := t

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
  val abs : t -> t
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
    val to_tuple : t -> (N.t * N.t) option
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
      | Interval (l, r) -> interval l r
      | Neg_Infinity r -> neg_infinity r
      | Pos_Infinity l -> pos_infinity l
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

    let to_tuple = function
      | Interval (l, r) -> Some (l, r)
      | Neg_Infinity r -> Some N.(neg_infinity, r)
      | Pos_Infinity l -> Some N.(l, infinity)
      | Infinity -> Some N.(neg_infinity, infinity)
      | Empty -> None
    ;;

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

    let search ~f ~eps interval =
      let global_median = median interval in
      (* TODO *)
      if N.(equal global_median (-one)) then ();
      let ends_difference =
        Interval.case
          ~infinity:N.(f one - f (-one))
          ~neg_infinity:(fun right -> N.(f right - f global_median))
          ~pos_infinity:(fun left -> N.(f global_median - f left))
          ~interval:(fun l r -> N.(f r - f l))
          ~empty:N.zero
          interval
      in
      let is_positive = N.(ends_difference > zero) in
      let is_negative = N.(ends_difference < zero) in
      let comparer = if is_positive then N.( > ) else N.( < ) in
      let is_interval_value_less_eps =
        match Interval.to_tuple interval with
        | Some (left, _) when N.(abs (f left) < eps) -> Some left
        | Some (_, right) when N.(abs (f right) < eps) -> Some right
        | _ -> None
      in
      let increase_interval ~f interval =
        let increase k rl cnd =
          let rec increase_rec k =
            let rl1 = N.(rl + k) in
            if cnd (f rl1) then rl1 else increase_rec N.(k * (one + one))
          in
          let t = increase_rec k in
          if N.(equal infinity t) then assert false else t
        in
        let lt_zero = N.(( > ) zero) in
        let gt_zero = N.(( < ) zero) in
        let res =
          Interval.case
            ~infinity:(assert false)
            ~neg_infinity:(assert false)
            ~pos_infinity:(assert false)
            ~interval:(fun l r -> l, r)
            ~empty:N.(one, zero)
            interval
        in
        Interval.of_tuple res
      in
      assert false
    in
    assert false
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
