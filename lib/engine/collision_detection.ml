open Core
open Open

let distance ~x1 ~y1 ~x2 ~y2 = Float.(sqrt (square (x2 - x1) + square (y2 - y1)))

let inter values ~global =
  let calc v =
    ExprCoef.calc
      ~values:(Values.to_function values)
      ~scoped_values:(Values.global_to_scoped global)
      (module Float)
      v
  in
  function
  | `Interval (l, r) -> `Interval (calc l, calc r)
  | `PosInfinity l -> `PosInfinity (calc l)
;;

let is_in_interval n = function
  | `Interval (l, r) -> Float.(l <= n && n < r)
  | `PosInfinity l -> Float.(l <= n)
;;

module WithBody = struct
  type extra =
    { id1 : Body.Id.t
    ; id2 : Body.Id.t
    ; roots_filtered : float list
    }
  [@@deriving sexp, equal]

  let collision_extra ~id1 ~id2 ~rules1 ~rules2 ~r ~values1 ~values2 ~global ~eps =
    Sequence.cartesian_product (Sequence.of_list rules1) (Sequence.of_list rules2)
    |> Sequence.map ~f:(fun ((a : Rule.t), (b : Rule.t)) ->
           let open Formula.Syntax in
           let x_1 = scope a.x ~scope:`_1 in
           let x_2 = scope b.x ~scope:`_2 in
           let y_1 = scope a.y ~scope:`_1 in
           let y_2 = scope b.y ~scope:`_2 in
           let r_1 = scope r ~scope:`_1 in
           let r_2 = scope r ~scope:`_2 in
           let f = sqr (x_2 - x_1) + sqr (y_2 - y_1) - sqr (r_1 + r_2) in
           let p =
             Formula.to_map f ~values:(Values.to_function global) ~scoped_values:(function
                 | `Global -> Values.to_function global
                 | `_1 -> Values.to_function values1
                 | `_2 -> Values.to_function values2)
             |> Solver.P.of_map ~eps
           in
           let roots = Solver.PE.roots p ~eps in
           let ai = inter values1 a.interval ~global in
           let bi = inter values2 b.interval ~global in
           let roots_filtered =
             roots
             |> List.filter ~f:(fun root ->
                    is_in_interval root ai && is_in_interval root bi)
           in
           { id1; id2; roots_filtered })
  ;;

  let distance_bentween_bodies values1 values2 =
    let x1 = Values.get_scalar_exn values1 ~var:`x0 in
    let y1 = Values.get_scalar_exn values1 ~var:`y0 in
    let x2 = Values.get_scalar_exn values2 ~var:`x0 in
    let y2 = Values.get_scalar_exn values2 ~var:`y0 in
    distance ~x1 ~y1 ~x2 ~y2
  ;;

  let first_collision (bodies : (_ * Body.t) Sequence.t) ~r ~global =
    let extra =
      let global_values = global in
      Sequence.cartesian_product bodies bodies
      |> Sequence.filter_map ~f:(fun ((id1, f1), (id2, f2)) ->
             let r1 = Values.get_scalar_exn f1.values ~var:`r in
             let r2 = Values.get_scalar_exn f2.values ~var:`r in
             let distance = distance_bentween_bodies f1.values f2.values in
             if Float.(r1 + r2 + eps < distance)
             then
               Some
                 (collision_extra
                    ~id1
                    ~id2
                    ~rules1:f1.rules
                    ~rules2:f2.rules
                    ~values1:f1.values
                    ~values2:f2.values
                    ~r
                    ~global:global_values
                    ~eps)
             else None)
      |> Sequence.concat
    in
    let%map.Option { id1; id2; _ }, t =
      extra
      |> Sequence.filter_map ~f:(fun a ->
             match List.min_elt a.roots_filtered ~compare:Float.compare with
             | None -> None
             | Some r -> Some (a, r))
      |> Sequence.min_elt ~compare:(fun (_r1, a) (_r2, b) -> Float.compare a b)
    in
    t, id1, id2
  ;;
end

module WithPoint = struct
  let distance_beetween_body_and_point ~(body : Body.t) ~point:Point.{ x = x2; y = y2 } =
    let x1 = Values.get_scalar_exn body.values ~var:`x0 in
    let y1 = Values.get_scalar_exn body.values ~var:`y0 in
    distance ~x1 ~y1 ~x2 ~y2
  ;;

  let collision ~rules ~point:Point.{ x; y } ~r ~values ~global ~eps =
    let f rule =
      let open Formula.Syntax in
      let x_1 = scope rule.Rule.x ~scope:`_1 in
      let y_1 = scope rule.y ~scope:`_1 in
      let x_2 =
        ExprCoef.Syntax.scalar_var (`with_point `x)
        |> Formula.singleton_zero
        |> scope ~scope:`_2
      in
      let y_2 =
        ExprCoef.Syntax.scalar_var (`with_point `y)
        |> Formula.singleton_zero
        |> scope ~scope:`_2
      in
      let r_1 = scope r ~scope:`_1 in
      let i = inter values rule.interval ~global in
      sqr (x_2 - x_1) + sqr (y_2 - y_1) - sqr r_1
      |> Formula.to_map ~values:(Values.to_function global) ~scoped_values:(function
             | `Global -> Values.to_function global
             | `_1 -> Values.to_function values
             | `_2 ->
               (function
               | `with_point `x -> x
               | `with_point `y -> y
               | _ -> assert false))
      |> Solver.P.of_map ~eps
      |> Solver.PE.roots ~eps
      |> List.filter ~f:(fun root -> is_in_interval root i)
      |> List.min_elt ~compare:Float.compare
    in
    rules |> Sequence.of_list |> Sequence.filter_map ~f |> Sequence.hd
  ;;

  let first_collision ~global bodies ~points ~r =
    let f ((id, body), point) =
      let distance = distance_beetween_body_and_point ~body ~point in
      let radius = Values.get_scalar_exn body.values ~var:`r in
      if Float.(radius + eps < distance)
      then (
        let%map.Option ret =
          collision ~rules:body.rules ~point ~r ~values:body.values ~global ~eps
        in
        ret, id, point)
      else None
    in
    Sequence.cartesian_product bodies points
    |> Sequence.filter_map ~f
    |> Sequence.min_elt ~compare:(fun (a, _, _) (b, _, _) -> Float.compare a b)
  ;;
end

module WithLine = struct
  let distance_beetween_body_and_line ~(body : Body.t) ~line =
    let a, b, c = Line.to_abc line in
    let x = Values.get_scalar_exn body.values ~var:`x0 in
    let y = Values.get_scalar_exn body.values ~var:`y0 in
    Float.(abs ((a * x) + (b * y) + c) / Float.sqrt (square a + square b))
  ;;

  let collision ~rules ~(line : Line.t) ~r ~values ~global ~eps =
    let f (rule : Rule.t) =
      let open Formula.Syntax in
      let x = scope rule.x ~scope:`_1 in
      let y = scope rule.y ~scope:`_1 in
      let r = scope r ~scope:`_1 in
      let a =
        ExprCoef.Syntax.scalar_var (`with_line `a)
        |> Formula.singleton_zero
        |> scope ~scope:`_2
      in
      let b =
        ExprCoef.Syntax.scalar_var (`with_line `b)
        |> Formula.singleton_zero
        |> scope ~scope:`_2
      in
      let c =
        ExprCoef.Syntax.scalar_var (`with_line `c)
        |> Formula.singleton_zero
        |> scope ~scope:`_2
      in
      let a2b2 =
        ExprCoef.Syntax.scalar_var (`with_line `a2b2)
        |> Formula.singleton_zero
        |> scope ~scope:`_2
      in
      let i = inter values rule.interval ~global in
      let f1 = (a * x) + (b * y) + c - (r * a2b2) in
      let f2 = (a * x) + (b * y) + c + (r * a2b2) in
      let a', b', c' = Line.to_abc line in
      let a2b2' = Float.(sqrt (square a' + square b')) in
      let to_polynomial =
        Formula.to_map ~values:(Values.to_function global) ~scoped_values:(function
            | `Global -> Values.to_function global
            | `_1 -> Values.to_function values
            | `_2 ->
              (function
              | `with_line `a -> a'
              | `with_line `b -> b'
              | `with_line `c -> c'
              | `with_line `a2b2 -> a2b2'
              | _ -> assert false))
        >> Solver.P.of_map ~eps
      in
      let abc = (a * x) + (b * y) + c |> to_polynomial in
      let is_qwf ~p1 ~p2 ~root =
        let x0 = x |> to_polynomial |> Solver.P.calc ~x:root in
        let y0 = y |> to_polynomial |> Solver.P.calc ~x:root in
        let Point.{ x = x1; y = y1 } = p1 in
        let Point.{ x = x2; y = y2 } = p2 in
        let v1 = Float.(x2 - x1, y2 - y1) in
        let v2 = Float.(x0 - x1, y0 - y1) in
        Float.(Vector.dot v1 v2 > zero)
      in
      let c f ~cond =
        f
        |> to_polynomial
        |> Solver.PE.roots ~eps
        |> List.filter ~f:(fun root ->
               is_in_interval root i
               && cond (Solver.P.calc abc ~x:root)
               &&
               match line.kind with
               | `Line -> true
               | `Ray -> is_qwf ~p1:line.p1 ~p2:line.p2 ~root
               | `Segment ->
                 is_qwf ~p1:line.p1 ~p2:line.p2 ~root
                 && is_qwf ~p1:line.p2 ~p2:line.p1 ~root)
      in
      c f1 ~cond:Float.(fun a -> a >= zero) @ c f2 ~cond:Float.(fun a -> a < zero)
      |> List.min_elt ~compare:Float.compare
    in
    rules |> Sequence.of_list |> Sequence.filter_map ~f |> Sequence.hd
  ;;

  let first_collision ~global bodies ~lines ~r =
    let f ((id, (body : Body.t)), line) =
      let distance = distance_beetween_body_and_line ~body ~line in
      let radius = Values.get_scalar_exn body.values ~var:`r in
      if Float.(radius + eps < distance)
      then (
        let%map.Option ret =
          collision ~rules:body.rules ~line ~r ~values:body.values ~global ~eps
        in
        ret, id, line)
      else None
    in
    Sequence.cartesian_product bodies lines
    |> Sequence.filter_map ~f
    |> Sequence.min_elt ~compare:(fun (a, _, _) (b, _, _) -> Float.compare a b)
  ;;
end
