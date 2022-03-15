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
  val sign_exn : t -> Sign.t
end) =
struct
  module Interval : sig
    type t =
      | Interval of
          { left : N.t
          ; right : N.t
          }
      | Neg_Infinity of { right : N.t }
      | Pos_Infinity of { left : N.t }
      | Infinity
      | Empty
    [@@deriving sexp, equal]

    val case
      :  interval:(left:N.t -> right:N.t -> 'a)
      -> neg_infinity:(right:N.t -> 'a)
      -> pos_infinity:(left:N.t -> 'a)
      -> infinity:(unit -> 'a)
      -> empty:(unit -> 'a)
      -> t
      -> 'a

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
      | Neg_Infinity of { right : N.t }
      | Pos_Infinity of { left : N.t }
      | Infinity
      | Empty
    [@@deriving sexp, equal, variants]

    let case ~interval ~neg_infinity ~pos_infinity ~infinity ~empty = function
      | Interval { left; right } -> interval ~left ~right
      | Neg_Infinity { right } -> neg_infinity ~right
      | Pos_Infinity { left } -> pos_infinity ~left
      | Infinity -> infinity ()
      | Empty -> empty ()
    ;;

    let create ~left:low ~right:high =
      if N.(equal low neg_infinity && equal high infinity)
      then Infinity
      else if N.(equal low neg_infinity)
      then Neg_Infinity { right = high }
      else if N.(equal high infinity)
      then Pos_Infinity { left = low }
      else if Int.(N.compare low high > 0)
      then Empty
      else Interval { left = low; right = high }
    ;;

    let of_tuple (left, right) = create ~left ~right

    let to_tuple = function
      | Interval { left; right } -> Some (left, right)
      | Neg_Infinity { right } -> Some N.(neg_infinity, right)
      | Pos_Infinity { left } -> Some N.(left, infinity)
      | Infinity -> Some N.(neg_infinity, infinity)
      | Empty -> None
    ;;

    let intervals_of_list roots =
      match roots with
      | [] -> [ infinity ]
      | [ a ] -> [ neg_infinity ~right:a; pos_infinity ~left:a ]
      | a :: b :: _ ->
        let w = roots |> Common.List.windowed2_exn |> List.map ~f:of_tuple in
        (neg_infinity ~right:a :: w) @ [ pos_infinity ~left:b ]
    ;;

    let difference = function
      | Interval { left; right } -> N.(abs (right - left))
      | Neg_Infinity _ | Pos_Infinity _ | Infinity -> N.infinity
      | Empty -> N.zero
    ;;

    let left_trunc = function
      | Interval { left; right = _ } | Pos_Infinity { left } -> left
      | Neg_Infinity _ | Infinity -> N.neg_infinity
      | Empty -> N.nan
    ;;

    let right_trunc = function
      | Interval { left = _; right } | Neg_Infinity { right } -> right
      | Infinity | Pos_Infinity _ -> N.infinity
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

    module Comparable_by_degree : Comparable.S with type t := t
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

    module T_by_degree = struct
      type nonrec t = t [@@deriving sexp]

      let compare a b = [%compare: int] a.degree b.degree
    end

    module Comparable_by_degree = Core.Comparable.Make (T_by_degree)

    let ( + ) a b =
      create ~coefficient:N.(coefficient a + coefficient b) ~degree:(degree a)
    ;;
  end

  module LinearEquation : sig
    val root : a:N.t -> b:N.t -> N.t
    val root_opt : a:N.t -> b:N.t -> N.t option
  end = struct
    let root ~a ~b = N.(-b / a)
    let root_opt ~a ~b = if N.(a = zero) then None else Some N.(-b / a)
  end

  module Polynomial : sig
    type t [@@deriving sexp, equal]

    val normalize : t -> t
    val of_list : Monomial.t list -> t
    val derivative : t -> t
    val calc : t -> x:N.t -> N.t
    val degree : t -> int
    val linear_root : t -> N.t option
    val rep_ok : t -> t
  end = struct
    type t = Monomial.t list [@@deriving sexp, equal]
    type t1 = (int, N.t, Int.comparator_witness) Map.t

    let normalize =
      let open Common.Fn in
      List.sort_and_group ~compare:Monomial.Comparable_by_degree.descending
      >> List.map ~f:(List.reduce_exn ~f:Monomial.( + ))
    ;;

    let rep_ok p =
      let normalized = normalize p in
      if equal normalized p
      then p
      else Error.raise_s [%message "RI violated" ~p:(p : t) ~normalized:(normalized : t)]
    ;;

    let of_list = normalize
    let t_of_sexp = Common.Fn.(t_of_sexp >> normalize)
    let derivative = List.map ~f:Monomial.derivative

    let calc poly ~x =
      poly |> List.map ~f:(Monomial.calc ~x) |> List.sum (module N) ~f:Fn.id
    ;;

    let degree = function
      | [] -> 0
      | hd :: _ -> Monomial.degree hd
    ;;

    let linear_root =
      let open Monomial in
      function
      | [ { coefficient = a; degree = 1 }; { coefficient = b; degree = 0 } ] ->
        Some (LinearEquation.root ~a ~b)
      | [ { coefficient = _; degree = 1 } ] -> Some N.zero
      | _ -> None
    ;;
  end

  module Binary_search1 = struct
    let two = N.(one + one)

    let rec search_rec ~f ~eps (xl, xr) =
      let xm = N.((xl + xr) / two) in
      let ym = f xm in
      if N.(abs ym < eps)
      then Some xm
      else if N.(ym < zero)
      then search_rec ~f ~eps (xm, xr)
      else search_rec ~f ~eps (xl, xm)
    ;;

    let rec increase_rec ~f ~eps ~t x k =
      if N.(f x < zero)
      then search_rec ~f ~eps (t ~x)
      else increase_rec ~f ~eps ~t N.(x + k) N.(k * two)
    ;;

    let search ~f ~eps =
      let rec search_rec ~f (xl, xr) =
        let xm = N.((xl + xr) / two) in
        let ym = f xm in
        if N.(abs ym < eps)
        then Some xm
        else if N.(ym < zero)
        then search_rec ~f (xm, xr)
        else search_rec ~f (xl, xm)
      in
      let rec increase_rec ~f ~t x k =
        if N.(f x < zero)
        then search_rec ~f (t ~x)
        else increase_rec ~f ~t N.(x + k) N.(k * two)
      in
      let make_f_pos ~yl ~yr = if N.(yl < yr) then f else fun a -> N.(-f a) in
      function
      | Interval.Interval { left = xl; right = xr } ->
        let yl, yr = f xl, f xr in
        if N.(Bool.equal (yl < zero) (yr < zero))
        then None
        else search_rec ~f:(make_f_pos ~yl ~yr) (xl, xr)
      | Neg_Infinity { right = xr } ->
        let xl1 = N.(xr - one) in
        let f = make_f_pos ~yl:(f xl1) ~yr:(f xr) in
        let yl1, yr = f xl1, f xr in
        if N.(yl1 < zero && yr < zero)
        then None
        else increase_rec ~f ~t:(fun ~x -> x, xr) xl1 N.one
      | Pos_Infinity { left = xl } ->
        let xr1 = N.(xl + one) in
        let yl, yr1 = f xl, f xr1 in
        let f = make_f_pos ~yl ~yr:yr1 in
        if N.(yl > zero && yr1 > zero)
        then None
        else increase_rec ~f ~t:(fun ~x -> xl, x) xr1 N.one
      | Infinity ->
        let xl1, xr1 = N.(-one, one) in
        let f = make_f_pos ~yl:(f xl1) ~yr:(f xr1) in
        if N.(f zero > zero)
        then increase_rec ~f ~t:(fun ~x -> x, N.zero) xl1 N.one
        else increase_rec ~f ~t:(fun ~x -> N.zero, x) xr1 N.one
      | Empty -> None
    ;;
  end

  module BinarySearch = struct
    let median =
      Interval.case
        ~infinity:(fun () -> N.zero)
        ~neg_infinity:(fun ~right -> N.(right - one))
        ~pos_infinity:(fun ~left -> N.(left + one))
        ~interval:(fun ~left ~right -> N.((left + right) / (one + one)))
        ~empty:(fun () -> N.zero)
    ;;

    let search ~f ~eps interval =
      let global_median = median interval in
      (* TODO *)
      if N.(equal global_median (-one)) then ();
      let ends_difference =
        Interval.case
          ~infinity:(fun () -> N.(f one - f (-one)))
          ~neg_infinity:(fun ~right -> N.(f right - f global_median))
          ~pos_infinity:(fun ~left -> N.(f global_median - f left))
          ~interval:(fun ~left ~right -> N.(f right - f left))
          ~empty:(fun () -> N.zero)
          interval
      in
      let sign =
        if N.(ends_difference > zero)
        then `Is_positive
        else if N.(ends_difference < zero)
        then `Is_negative
        else (* TODO *) `Is_positive
      in
      let comparer =
        match sign with
        | `Is_positive -> N.( > )
        | `Is_negative -> N.( < )
      in
      let is_interval_value_less_eps =
        match Interval.to_tuple interval with
        | Some (left, _) when N.(abs (f left) < eps) -> Some left
        | Some (_, right) when N.(abs (f right) < eps) -> Some right
        | _ -> None
      in
      let increase_interval ~f interval =
        let increase k rl cnd =
          let rec increase_rec cnt k =
            if cnt > 1000
            then
              Error.raise_s
                [%message
                  "loop"
                    ~interval:(interval : Interval.t)
                    ~rl:(rl : N.t)
                    ~cnt:(cnt : int)
                    ~k:(k : N.t)]
            else (
              let rl1 = N.(rl + k) in
              if cnd (f rl1) then rl1 else increase_rec (cnt + 1) N.(k * (one + one)))
          in
          let t = increase_rec 0 k in
          if N.(equal infinity t) then (* TODO *)
                                    assert false else t
        in
        let lt_zero a = N.(zero > a) in
        let gt_zero a = N.(zero < a) in
        let res =
          Interval.case
            ~infinity:(fun () ->
              match sign, N.zero |> f |> N.sign_exn with
              | `Is_positive, (Pos | Zero) -> increase N.(-one) N.zero lt_zero, N.zero
              | `Is_negative, (Pos | Zero) -> N.zero, increase N.(one) N.zero lt_zero
              | `Is_positive, Neg -> N.zero, increase N.(one) N.zero gt_zero
              | `Is_negative, Neg -> increase N.(-one) N.zero gt_zero, N.zero)
            ~neg_infinity:(fun ~right ->
              match sign with
              | `Is_positive -> increase N.(-one) right lt_zero, right
              | `Is_negative -> increase N.(-one) right gt_zero, right)
            ~pos_infinity:(fun ~left ->
              match sign with
              | `Is_positive -> left, increase N.(one) left gt_zero
              | `Is_negative -> left, increase N.(one) left lt_zero)
            ~interval:(fun ~left ~right -> left, right)
            ~empty:(fun () -> N.(one, zero))
            interval
        in
        Interval.of_tuple res
      in
      let increased_interval = increase_interval ~f interval in
      let rec rec_search cnt interval =
        if cnt > 100
        then failwith "loop"
        else (
          let local_median = median interval in
          let median_value = f local_median in
          if N.(Interval.difference interval < eps)
          then local_median
          else if comparer median_value N.zero
          then
            rec_search (cnt + 1)
            @@ Interval.create ~left:(Interval.left_trunc interval) ~right:local_median
          else
            rec_search (cnt + 1)
            @@ Interval.create ~left:local_median ~right:(Interval.right_trunc interval))
      in
      match is_interval_value_less_eps with
      | Some x -> x
      | None -> rec_search 0 increased_interval
    ;;
  end

  module PolynomialEquation : sig
    val roots : eps:N.t -> Polynomial.t -> N.t list
  end = struct
    let filter_intervals
        : eps:N.t -> calc:(N.t -> N.t) -> Interval.t list -> Interval.t list
      =
     fun ~eps ~calc intervals ->
      List.filter
        intervals
        ~f:
          (Interval.case
             ~neg_infinity:(fun ~right ->
               let a, b = calc N.(right - one), calc right in
               not N.((a > b && b > eps) || (a < b && b < -eps)))
             ~pos_infinity:(fun ~left ->
               let a, b = calc left, calc N.(left + one) in
               not N.((a > b && a < -eps) || (a < b && a > eps)))
             ~interval:(fun ~left ~right ->
               let a, b = calc left, calc right in
               not N.(Sign.(sign_exn a * sign_exn b = Pos) && abs (a * b) >= eps))
             ~infinity:(fun () -> false)
             ~empty:(fun () -> false))
   ;;

    let rec roots ~eps poly =
      match Polynomial.degree poly with
      | neg when neg <= 0 -> []
      | 1 -> poly |> Polynomial.linear_root |> Option.to_list
      | 2 ->
        let calc x = Polynomial.calc poly ~x in
        let d = poly |> Polynomial.derivative in
        let d_roots = d |> Polynomial.linear_root |> Option.to_list in
        d_roots
        |> Interval.intervals_of_list
        |> filter_intervals ~eps ~calc
        |> List.map ~f:(BinarySearch.search ~f:calc ~eps)
        |> List.sort ~compare:N.ascending
      | _ ->
        let calc x = Polynomial.calc poly ~x in
        poly
        |> Polynomial.derivative
        |> roots ~eps
        |> Interval.intervals_of_list
        |> filter_intervals ~eps ~calc
        |> List.map ~f:(BinarySearch.search ~f:calc ~eps)
        |> List.sort ~compare:N.ascending
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
