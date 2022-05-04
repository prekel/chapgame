open Core

module Make
    (N : Module_types.NUMBER)
    (Interval : module type of Interval.Make (N)) =
    struct
  let two = N.(one + one)

  let search ~f ~eps =
    let rec search_rec cnt ~f (xl, xr) =
      assert (N.(f xl < f xr));
      if cnt > 50000
      then begin
        print_s
          [%message
            "search_rec"
              ~cnt:(cnt : int)
              ~xl:(xl : N.t)
              ~xr:(xr : N.t)
              ~f:(f xl : N.t)
              ~eps:(eps : N.t)];
        Some N.((xl + xr) / two)
      end
      else (
        (* TODO *)
        (* Error.raise_s [%message "search_rec" ~cnt:(cnt : int) ~xl:(xl : N.t) ~xr:(xr :
           N.t) ~f:(f xl : N.t) ~poly:(poly : Polynomial.t) ~eps:(eps : N.t)]; *)
        let xm = N.((xl + xr) / two) in
        let ym = f xm in
        if N.(abs ym < eps || (Int.(cnt > 40000) && xr - xl < eps))
        then Some xm
        else if N.(ym < zero)
        then search_rec (cnt + 1) ~f (xm, xr)
        else search_rec (cnt + 1) ~f (xl, xm))
    in
    let search_rec = search_rec 0 in
    let rec increase_rec cnt ~f ~t ~y x k =
      if cnt > 125
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
