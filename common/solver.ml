open Core

module MakeSolver (N : Module_types.Number) = struct
  module Interval : sig
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
  end = struct
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
        (neg_infinity ~right:a
        :: (Common.List.windowed2_exn roots |> List.map ~f:of_tuple))
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

  module Monomial : sig
    type t =
      { coefficient : N.t
      ; degree : int
      }
    [@@deriving sexp, compare, equal]

    val degree : t -> int
    val coefficient : t -> N.t
    val create : coefficient:N.t -> degree:int -> t
    val derivative : t -> t
    val calc : t -> x:N.t -> N.t
    val ( + ) : t -> t -> t

    module ComparableByDegree : Comparable.S with type t := t
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
      | 0 -> { coefficient = zero; degree = 0 }
      | _ ->
        { coefficient = x.coefficient * of_int x.degree; degree = Int.(x.degree - 1) }
    ;;

    let calc mono ~x =
      let open N in
      mono.coefficient * (x ** of_int mono.degree)
    ;;

    module TDegree = struct
      type nonrec t = t [@@deriving sexp]

      let compare a b = [%compare: int] a.degree b.degree
    end

    module ComparableByDegree = Core.Comparable.Make (TDegree)

    let ( + ) a b =
      create ~coefficient:N.(coefficient a + coefficient b) ~degree:(degree a)
    ;;
  end

  module Polynomial : sig
    type t [@@deriving sexp, equal]

    val of_list : (int * N.t) list -> t
    val derivative : t -> t
    val calc : t -> x:N.t -> N.t
    val degree : t -> int
    val to_map : t -> (int, N.t, Int.comparator_witness) Map.t
    val of_map : (int, N.t, Int.comparator_witness) Map.t -> t
    val to_string_hum : ?var:string -> t -> string
  end = struct
    type t = (int, N.t, Int.comparator_witness) Map.t

    let equal = Map.equal N.equal
    let normalize = Map.filter ~f:N.(( <> ) zero)
    let of_list a = Map.of_alist_exn (module Int) a |> normalize
    let t_of_sexp s = List.Assoc.t_of_sexp Int.t_of_sexp N.t_of_sexp s |> of_list
    let sexp_of_t t = [%sexp (Map.to_alist t : (int * N.t) list)]

    let derivative p =
      Map.filter_mapi p ~f:(fun ~key:degree ~data:coefficient ->
          match degree with
          | 0 -> None
          | _ -> Some N.(coefficient * of_int degree))
      |> Map.map_keys_exn (module Int) ~f:(fun a -> a - 1)
    ;;

    let calc poly ~x =
      Map.fold poly ~init:N.zero ~f:(fun ~key:degree ~data:coef acc ->
          N.(acc + (coef * (x ** of_int degree))))
    ;;

    let degree p =
      match Map.max_elt p with
      | Some (deg, _) -> deg
      | None -> 0
    ;;

    let to_map = Fn.id
    let of_map = normalize

    let to_string_hum ?var =
      let var = Option.value ~default:"x" var in
      Map.fold_right ~init:"" ~f:(fun ~key:degree ~data:coef acc ->
          acc
          ^ N.to_string_hum ~strip_zero:true ~explicit_plus:String.(acc <> "") coef
          ^ var
          ^ "^"
          ^ Int.to_string degree)
    ;;
  end

  module LinearEquation : sig
    val root : a:N.t -> b:N.t -> N.t
    val root_opt : a:N.t -> b:N.t -> N.t option
    val root_poly : Polynomial.t -> N.t option
  end = struct
    let root ~a ~b = N.(-b / a)
    let root_opt ~a ~b = if N.(a = zero) then None else Some N.(-b / a)

    let root_poly poly =
      let poly = Polynomial.to_map poly in
      match Map.find poly 1, Map.find poly 0 with
      | Some a, Some b -> Some (root ~a ~b)
      | Some a, None -> Some (root ~a ~b:N.zero)
      | _, _ -> None
    ;;
  end

  module BinarySearch = struct
    let two = N.(one + one)

    let search ~f ~eps =
      let rec search_rec cnt ~f (xl, xr) =
        assert (N.(f xl < f xr));
        if cnt > 100
        then
          Error.raise_s
            [%message
              "search_rec" ~cnt:(cnt : int) ~xl:(xl : N.t) ~xr:(xr : N.t) ~f:(f xl : N.t)];
        let xm = N.((xl + xr) / two) in
        let ym = f xm in
        if N.(abs ym < eps)
        then Some xm
        else if N.(ym < zero)
        then search_rec (cnt + 1) ~f (xm, xr)
        else search_rec (cnt + 1) ~f (xl, xm)
      in
      let search_rec = search_rec 0 in
      let rec increase_rec cnt ~f ~t ~y x k =
        if cnt > 100
        then
          Error.raise_s
            [%message
              "increase_rec" ~cnt:(cnt : int) ~x:(x : N.t) ~k:(k : N.t) ~y:(y : N.t)];
        if Sign.(N.(sign_exn (f x)) <> N.(sign_exn y))
        then search_rec ~f (t ~x)
        else increase_rec (cnt + 1) ~f ~t ~y N.(x + k) N.(k * two)
      in
      let increase_rec = increase_rec 0 in
      function
      | Interval.Interval { left = xl; right = xr } ->
        let f = if N.(f xl < f xr) then f else N.(fun x -> -f x) in
        if Bool.(N.(f xl < zero) = N.(f xr < zero)) then None else search_rec ~f (xl, xr)
      | NegInfinity { right = xr } ->
        let xl1 = N.(xr - one) in
        let f = if N.(f xl1 < f xr) then f else N.(fun x -> -f x) in
        if N.(f xl1 < zero && f xr < zero)
        then None
        else increase_rec ~f ~t:(fun ~x -> x, xr) ~y:(f xr) xl1 N.(-one)
      | PosInfinity { left = xl } ->
        let xr1 = N.(xl + one) in
        let f = if N.(f xl < f xr1) then f else N.(fun x -> -f x) in
        if N.(f xl > zero && f xr1 > zero)
        then None
        else increase_rec ~f ~t:(fun ~x -> xl, x) ~y:(f xl) xr1 N.one
      | Infinity ->
        let xl1, xr1 = N.(-one, one) in
        let f = if N.(f xl1 < f xr1) then f else N.(fun x -> -f x) in
        if N.(f zero > zero)
        then increase_rec ~f ~t:(fun ~x -> x, N.zero) ~y:(f xr1) xl1 N.(-one)
        else increase_rec ~f ~t:(fun ~x -> N.zero, x) ~y:(f xl1) xr1 N.one
      | Empty -> None
    ;;
  end

  module PolynomialEquation : sig
    val roots : eps:N.t -> Polynomial.t -> N.t list
  end = struct
    let rec roots ~eps poly =
      match Polynomial.degree poly with
      | neg when neg <= 0 -> []
      | 1 -> poly |> LinearEquation.root_poly |> Option.to_list
      | _ ->
        let calc x = Polynomial.calc poly ~x in
        poly
        |> Polynomial.derivative
        |> roots ~eps
        |> Interval.intervals_of_list
        |> List.filter_map ~f:(BinarySearch.search ~f:calc ~eps)
        |> List.sort ~compare:N.ascending
    ;;
  end
end
