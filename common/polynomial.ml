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
    type t [@@deriving sexp]

    val create : N.t -> N.t -> t
    val of_tuple : N.t * N.t -> t
    val infinity : t
    val neg_infinity : N.t -> t
    val pos_infinity : N.t -> t
    val intervals_of_list : N.t list -> t list
  end = struct
    type t =
      | Interval of N.t * N.t
      | Empty
    [@@deriving sexp]

    let create low high =
      if Int.(N.compare low high > 0) then Empty else Interval (low, high)
    ;;

    let of_tuple (low, high) = create low high
    let infinity = N.(create neg_infinity infinity)
    let neg_infinity a = N.(create neg_infinity a)
    let pos_infinity a = N.(create a infinity)

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

  module LinearEquation : sig
    val solve : a:N.t -> b:N.t -> N.t
  end = struct
    let solve ~a ~b = N.(-b / a)
  end

  module Polynomial : sig
    type t [@@deriving sexp, equal]

    val normalize : t -> t
    val of_list : Monomial.t list -> t
    val derivative : t -> t
    val calc : N.t list -> Monomial.t -> N.t
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

    let calc poly x =
      poly |> List.map ~f:(Monomial.calc x) |> List.sum (module N) ~f:Fn.id
    ;;

    let degree = Common.Fn.(List.hd_exn >> Monomial.degree)

    let linear_root =
      let open Monomial in
      function
      | [ x0; x1 ] when degree x0 = 0 && degree x1 = 1 ->
        let b = coefficient x0 in
        let a = coefficient x1 in
        Some (LinearEquation.solve ~a ~b)
      | _ -> None
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
