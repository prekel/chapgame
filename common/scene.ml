open Core

module Make (N : Module_types.Number) (C : Module_types.Constants with module N = N) =
struct
  module Vars :
    Identifiable.S
      with type t =
        [ `x0
        | `y0
        | `v0_x
        | `v0_y
        | `r
        | `g
        | `mu
        | `m
        | `with_point of [ `x | `y ]
        | `with_line of [ `a | `b | `c | `a2b2 ]
        ] = struct
    module T = struct
      type t =
        [ `x0
        | `y0
        | `v0_x
        | `v0_y
        | `r
        | `g
        | `mu
        | `m
        | `with_point of [ `x | `y ]
        | `with_line of [ `a | `b | `c | `a2b2 ]
        ]
      [@@deriving bin_io, hash, compare, sexp]

      include Sexpable.To_stringable (struct
        type nonrec t = t [@@deriving sexp]
      end)

      let module_name = "Vars"
    end

    include T
    include Identifiable.Make (T)
  end

  module Scope = Int
  module Solver = Solver.MakeSolver (N)
  module Expr = Expr.Make (Vars) (Scope) (N)
  module Formula = Formula.Make (Vars) (Scope) (N) (Expr) (Solver)

  let eps = C.eps

  module Vector : sig
    include module type of Expr.VectorOps

    val dot : t -> t -> N.t
    val len_sqr : t -> N.t
    val len : t -> N.t
    val ( *^ ) : t -> N.t -> t
    val ( ^* ) : N.t -> t -> t
  end = struct
    include Expr.VectorOps

    let dot a b =
      let x, y = a * b in
      N.(x + y)
    ;;

    let len_sqr (x, y) = N.((x * x) + (y * y))
    let len a = N.(sqrt (len_sqr a))
    let ( *^ ) (a, b) c = N.(a * c, b * c)
    let ( ^* ) c (a, b) = N.(a * c, b * c)
  end

  let global_scope : Scope.t = -1

  module Values : sig
    type t [@@deriving sexp, equal]

    val get_scalar_exn : t -> var:Vars.t -> N.t
    val get_vector_exn : t -> var_x:Vars.t -> var_y:Vars.t -> N.t * N.t
    val update_scalar : t -> var:Vars.t -> value:N.t -> t
    val update_vector : t -> var_x:Vars.t -> var_y:Vars.t -> value:N.t * N.t -> t
    val of_alist : (Vars.t * N.t) list -> t
    val to_function : t -> Vars.t -> N.t
    val global_to_scoped : t -> Scope.t -> Vars.t -> N.t

    module Diff :
      Utils.AdvancedMapDiff with type t = t and type key = Vars.t and type value = N.t
  end = struct
    include Utils.MakeAdvancedMap (Vars) (N)

    let get_scalar_exn values ~var = Map.find_exn values var

    let get_vector_exn values ~var_x ~var_y =
      let x = Map.find_exn values var_x in
      let y = Map.find_exn values var_y in
      x, y
    ;;

    let update_scalar values ~var ~value = Map.update values var ~f:(fun _ -> value)

    let update_vector values ~var_x ~var_y ~value =
      let wx = Map.update values var_x ~f:(fun _ -> fst value) in
      let wy = Map.update wx var_y ~f:(fun _ -> snd value) in
      wy
    ;;

    let of_alist = of_alist_exn
    let to_function = find_exn

    let global_to_scoped g s =
      if Scope.(equal global_scope s) then to_function g else assert false
    ;;
  end

  module Figure2 = struct
    module Id : sig
      include Identifiable.S with type t = int

      val next : unit -> t
    end = struct
      module T = struct
        type t = int [@@deriving bin_io, hash, compare, sexp]

        include Sexpable.To_stringable (struct
          type nonrec t = t [@@deriving sexp]
        end)

        let module_name = "Figure.Id"
      end

      include T
      include Identifiable.Make (T)

      let current = ref 0

      let next () =
        let ret = !current in
        current := ret + 1;
        ret
      ;;
    end

    module Rule = struct
      type t =
        { interval :
            [ `Interval of Expr.t_scalar * Expr.t_scalar | `PosInfinity of Expr.t_scalar ]
        ; x : Formula.t
        ; y : Formula.t
        ; v_x : Formula.t
        ; v_y : Formula.t
        ; after : t list
        ; name : string
        }
      [@@deriving equal]

      module Exprs = struct
        open Expr.Syntax

        let g = scope ~scope:global_scope (scalar_var `g)
        let mu = scalar_var `mu
        let v0_vec = vector_var `v0_x `v0_y

        let a_vec =
          let f = mu * g in
          -vector_unit v0_vec * vector_of_scalar f f
        ;;

        let a_x = vector_x a_vec
        let a_y = vector_y a_vec
        let v0_x = vector_x v0_vec
        let v0_y = vector_y v0_vec
        let x0 = scalar_var `x0
        let y0 = scalar_var `y0
        let r = scalar_var `r
        let half = scalar_const N.(one / (one + one))
        let zero = scalar_const N.zero
        let three = scalar_const N.(one + one + one)
        let two = scalar_const N.(one + one)
        let pi = scalar_const N.pi
      end

      let rec rules1 =
        Expr.Syntax.
          [ { interval =
                `Interval Exprs.(zero, vector_length v0_vec / vector_length a_vec)
            ; x = Formula.of_alist_exn Exprs.[ 0, x0; 1, v0_x; 2, a_x / two ]
            ; y = Formula.of_alist_exn Exprs.[ 0, y0; 1, v0_y; 2, a_y / two ]
            ; v_x = Formula.of_alist_exn Exprs.[ 0, v0_x; 1, a_x ]
            ; v_y = Formula.of_alist_exn Exprs.[ 0, v0_y; 1, a_y ]
            ; after = rules1
            ; name = "rules1 - 0"
            }
          ; { interval = `PosInfinity Exprs.(vector_length v0_vec / vector_length a_vec)
            ; x =
                Formula.of_alist_exn
                  Exprs.
                    [ ( 0
                      , x0
                        + (v0_x * vector_length v0_vec / vector_length a_vec)
                        + (a_x
                          * sqr (vector_length v0_vec)
                          / (two * sqr (vector_length a_vec))) )
                    ]
            ; y =
                Formula.of_alist_exn
                  Exprs.
                    [ ( 0
                      , y0
                        + (v0_y * vector_length v0_vec / vector_length a_vec)
                        + (a_y
                          * sqr (vector_length v0_vec)
                          / (two * sqr (vector_length a_vec))) )
                    ]
            ; v_x = Formula.of_alist_exn Exprs.[ 0, zero ]
            ; v_y = Formula.of_alist_exn Exprs.[ 0, zero ]
            ; after = rules0
            ; name = "rules1 - 1"
            }
          ]

      and rules0 =
        [ { interval = `PosInfinity Exprs.zero
          ; x = Formula.of_alist_exn Exprs.[ 0, x0 ]
          ; y = Formula.of_alist_exn Exprs.[ 0, y0 ]
          ; v_x = Formula.of_alist_exn Exprs.[ 0, zero ]
          ; v_y = Formula.of_alist_exn Exprs.[ 0, zero ]
          ; after = rules0
          ; name = "rules0 - 0"
          }
        ]
      ;;

      let rules1_0, rules1_1, rules0_0 =
        match rules1, rules0 with
        | [ rules1_0; rules1_1 ], [ rules0_0 ] -> rules1_0, rules1_1, rules0_0
        | _ -> assert false
      ;;

      let sexp_of_t { name; _ } = Sexp.Atom name

      let t_of_sexp = function
        | Sexp.Atom "rules1 - 0" -> rules1_0
        | Sexp.Atom "rules1 - 1" -> rules1_1
        | Sexp.Atom "rules0 - 0" -> rules0_0
        | _ -> assert false
      ;;
    end

    type t =
      { id : Id.t
      ; values : Values.t
      ; rules : Rule.t list
      }
    [@@deriving sexp, equal]

    let calc ~values ~rules ~scoped_values ~t =
      let c = Expr.calc ~values ~scoped_values (module N) in
      let calc_xy f =
        Formula.to_polynomial f ~values ~scoped_values ~eps |> Solver.Polynomial.calc ~x:t
      in
      List.find_map rules ~f:(fun Rule.{ interval; x; y; v_x; v_y; after; _ } ->
          match interval with
          | `Interval (l, r) when N.(c l <= t && t < c r) ->
            Some ((calc_xy x, calc_xy y, calc_xy v_x, calc_xy v_y), after)
          | `PosInfinity l when N.(c l <= t) ->
            Some ((calc_xy x, calc_xy y, calc_xy v_x, calc_xy v_y), after)
          | _ -> None)
    ;;

    let update_x0y0 ~body (x, y, v_x, v_y) ~rules =
      { body with
        values =
          body.values
          |> Values.update_scalar ~var:`x0 ~value:x
          |> Values.update_scalar ~var:`y0 ~value:y
          |> Values.update_vector ~var_x:`v0_x ~var_y:`v0_y ~value:(v_x, v_y)
      ; rules
      }
    ;;

    let update_v0 body ~v ~rules =
      { body with
        values = body.values |> Values.update_vector ~var_x:`v0_x ~var_y:`v0_y ~value:v
      ; rules
      }
    ;;
  end

  module Point = struct
    module T = struct
      type t =
        { x : N.t
        ; y : N.t
        }
      [@@deriving sexp, equal, compare]
    end

    include T
    include Comparable.Make (T)
  end

  module Points = Utils.MakeAdvancedSet (Point)

  module LineSegmentRay = struct
    module T = struct
      type t =
        { p1 : Point.t
        ; p2 : Point.t
        ; kind : [ `Line | `Segment | `Ray ]
        }
      [@@deriving sexp, equal, compare]
    end

    include T
    include Comparable.Make (T)

    let of_points ~p1 ~p2 ~kind = { p1; p2; kind }

    let to_abc { p1; p2; kind = _ } =
      N.(p2.y - p1.y, p1.x - p2.x, (p1.y * p2.x) - (p1.x * p2.y))
    ;;
  end

  module Lines = Utils.MakeAdvancedSet (LineSegmentRay)

  module CollisionDetection : sig
    module WithBody : sig
      val first_collision
        :  (Figure2.Id.t * Figure2.t) Sequence.t
        -> r:Formula.t
        -> global:Values.t
        -> a:Expr.vector Expr.t
        -> (N.t * Figure2.Id.t * Figure2.Id.t) option
    end

    module WithPoint : sig
      val first_collision
        :  global:Values.t
        -> (Figure2.Id.t * Figure2.t) Sequence.t
        -> points:Point.t Sequence.t
        -> r:Formula.t
        -> (N.t * Figure2.Id.t * Point.t) option
    end

    module WithLine : sig
      val first_collision
        :  global:Values.t
        -> (Figure2.Id.t * Figure2.t) Sequence.t
        -> lines:LineSegmentRay.t Sequence.t
        -> r:Formula.t
        -> (N.t * Figure2.Id.t * LineSegmentRay.t) option
    end
  end = struct
    let distance ~x1 ~y1 ~x2 ~y2 = N.(sqrt (square (x2 - x1) + square (y2 - y1)))

    let inter values ~global =
      let calc v =
        Expr.calc
          ~values:(Values.to_function values)
          ~scoped_values:(Values.global_to_scoped global)
          (module N)
          v
      in
      function
      | `Interval (l, r) -> `Interval (calc l, calc r)
      | `PosInfinity l -> `PosInfinity (calc l)
    ;;

    let is_in_interval n = function
      | `Interval (l, r) -> N.(l <= n && n < r)
      | `PosInfinity l -> N.(l <= n)
    ;;

    module WithBody = struct
      type extra =
        { id1 : Figure2.Id.t
        ; id2 : Figure2.Id.t
        ; p : Solver.Polynomial.t
        ; f : Formula.t
        ; roots : N.t list
        ; roots_filtered : N.t list
        ; rule1 : Figure2.Rule.t
        ; rule2 : Figure2.Rule.t
        ; values1 : Values.t
        ; values2 : Values.t
        ; a1 : N.t * N.t
        ; a2 : N.t * N.t
        }
      [@@deriving sexp, equal]

      let collision_extra ~id1 ~id2 ~rules1 ~rules2 ~r ~ac ~values1 ~values2 ~global ~eps =
        Sequence.cartesian_product (Sequence.of_list rules1) (Sequence.of_list rules2)
        |> Sequence.map ~f:(fun (a, b) ->
               let open Formula.Syntax in
               let x_1 = scope a.Figure2.Rule.x ~scope:1 in
               let x_2 = scope b.Figure2.Rule.x ~scope:2 in
               let y_1 = scope a.y ~scope:1 in
               let y_2 = scope b.y ~scope:2 in
               let r_1 = scope r ~scope:1 in
               let r_2 = scope r ~scope:2 in
               let f = sqr (x_2 - x_1) + sqr (y_2 - y_1) - sqr (r_1 + r_2) in
               let ai = inter values1 a.interval ~global in
               let bi = inter values2 b.interval ~global in
               let p =
                 Formula.to_polynomial
                   f
                   ~eps
                   ~values:(Values.to_function global)
                   ~scoped_values:(function
                     | s when s = global_scope -> Values.to_function global
                     | 1 -> Values.to_function values1
                     | 2 -> Values.to_function values2
                     | _ -> assert false)
               in
               let roots = Solver.PolynomialEquation.roots p ~eps in
               let roots_filtered =
                 roots
                 |> List.filter ~f:(fun root ->
                        is_in_interval root ai && is_in_interval root bi)
               in
               { id1
               ; id2
               ; p
               ; f
               ; roots
               ; roots_filtered
               ; rule1 = a
               ; rule2 = b
               ; values1
               ; values2
               ; a1 =
                   Expr.calc
                     ~values:(Values.to_function values1)
                     ~scoped_values:(Values.global_to_scoped global)
                     (module Expr.VectorOps)
                     ac
               ; a2 =
                   Expr.calc
                     ~values:(Values.to_function values2)
                     ~scoped_values:(Values.global_to_scoped global)
                     (module Expr.VectorOps)
                     ac
               })
      ;;

      let distance_bentween_bodies values1 values2 =
        let x1 = Values.get_scalar_exn values1 ~var:`x0 in
        let y1 = Values.get_scalar_exn values1 ~var:`y0 in
        let x2 = Values.get_scalar_exn values2 ~var:`x0 in
        let y2 = Values.get_scalar_exn values2 ~var:`y0 in
        distance ~x1 ~y1 ~x2 ~y2
      ;;

      let collisions_extra ~eps ~global_values (bodies : (_ * Figure2.t) Sequence.t) ~r ~a
        =
        Sequence.cartesian_product bodies bodies
        |> Sequence.filter_map ~f:(fun ((id1, f1), (id2, f2)) ->
               let r1 = Values.get_scalar_exn f1.values ~var:`r in
               let r2 = Values.get_scalar_exn f2.values ~var:`r in
               let distance = distance_bentween_bodies f1.values f2.values in
               if N.(
                    r1 + r2 + eps < distance
                    (* || let v0_1 = Values.get_vector_exn f1.values ~var_x:`v0_x
                       ~var_y:`v0_y in let v0_2 = Values.get_vector_exn f2.values
                       ~var_x:`v0_x ~var_y:`v0_y in Vector.( len_sqr v0_1 > N.zero &&
                       len_sqr v0_2 > N.zero && dot v0_1 v0_2 > N.zero && len_sqr v0_2 >
                       len_sqr v0_1) *))
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
                      ~eps
                      ~ac:a)
               else None)
        |> Sequence.concat
      ;;

      let first_collision bodies ~r ~global ~a =
        let extra = collisions_extra ~eps ~global_values:global bodies ~r ~a in
        let%map.Option { id1; id2; _ }, t =
          extra
          |> Sequence.filter_map ~f:(fun a ->
                 match List.min_elt a.roots_filtered ~compare:N.compare with
                 | None -> None
                 | Some r -> Some (a, r))
          |> Sequence.min_elt ~compare:(fun (_r1, a) (_r2, b) -> N.compare a b)
        in
        t, id1, id2
      ;;
    end

    module WithPoint = struct
      let distance_beetween_body_and_point
          ~(body : Figure2.t)
          ~point:Point.{ x = x2; y = y2 }
        =
        let x1 = Values.get_scalar_exn body.values ~var:`x0 in
        let y1 = Values.get_scalar_exn body.values ~var:`y0 in
        distance ~x1 ~y1 ~x2 ~y2
      ;;

      let collision ~rules ~point:Point.{ x; y } ~r ~values ~global ~eps =
        rules
        |> Sequence.of_list
        |> Sequence.filter_map ~f:(fun (rule : Figure2.Rule.t) ->
               let open Formula.Syntax in
               let x_1 = scope rule.x ~scope:1 in
               let y_1 = scope rule.y ~scope:1 in
               let x_2 =
                 Expr.Syntax.scalar_var (`with_point `x)
                 |> Formula.singleton_zero
                 |> scope ~scope:2
               in
               let y_2 =
                 Expr.Syntax.scalar_var (`with_point `y)
                 |> Formula.singleton_zero
                 |> scope ~scope:2
               in
               let r_1 = scope r ~scope:1 in
               let i = inter values rule.interval ~global in
               sqr (x_2 - x_1) + sqr (y_2 - y_1) - sqr r_1
               |> Formula.to_polynomial
                    ~eps
                    ~values:(Values.to_function global)
                    ~scoped_values:(function
                      | s when s = global_scope -> Values.to_function global
                      | 1 -> Values.to_function values
                      | 2 ->
                        begin
                          function
                          | `with_point `x -> x
                          | `with_point `y -> y
                          | _ -> assert false
                        end
                      | _ -> assert false)
               |> Solver.PolynomialEquation.roots ~eps
               |> List.filter ~f:(fun root -> is_in_interval root i)
               |> List.min_elt ~compare:N.compare)
        |> Sequence.hd
      ;;

      let first_collision ~global bodies ~points ~r =
        Sequence.cartesian_product bodies points
        |> Sequence.filter_map ~f:(fun ((id, body), point) ->
               let distance = distance_beetween_body_and_point ~body ~point in
               let radius = Values.get_scalar_exn body.values ~var:`r in
               if N.(radius + eps < distance)
               then (
                 let%map.Option ret =
                   collision ~rules:body.rules ~point ~r ~values:body.values ~global ~eps
                 in
                 ret, id, point)
               else None)
        |> Sequence.min_elt ~compare:(fun (a, _, _) (b, _, _) -> N.compare a b)
      ;;
    end

    module WithLine = struct
      let distance_beetween_body_and_line ~(body : Figure2.t) ~line =
        let a, b, c = LineSegmentRay.to_abc line in
        let x = Values.get_scalar_exn body.values ~var:`x0 in
        let y = Values.get_scalar_exn body.values ~var:`y0 in
        N.(abs ((a * x) + (b * y) + c) / N.sqrt (square a + square b))
      ;;

      let collision ~rules ~(line : LineSegmentRay.t) ~r ~values ~global ~eps =
        rules
        |> Sequence.of_list
        |> Sequence.filter_map ~f:(fun (rule : Figure2.Rule.t) ->
               let open Formula.Syntax in
               let x = scope rule.x ~scope:1 in
               let y = scope rule.y ~scope:1 in
               let r = scope r ~scope:1 in
               let a =
                 Expr.Syntax.scalar_var (`with_line `a)
                 |> Formula.singleton_zero
                 |> scope ~scope:2
               in
               let b =
                 Expr.Syntax.scalar_var (`with_line `b)
                 |> Formula.singleton_zero
                 |> scope ~scope:2
               in
               let c =
                 Expr.Syntax.scalar_var (`with_line `c)
                 |> Formula.singleton_zero
                 |> scope ~scope:2
               in
               let a2b2 =
                 Expr.Syntax.scalar_var (`with_line `a2b2)
                 |> Formula.singleton_zero
                 |> scope ~scope:2
               in
               let i = inter values rule.interval ~global in
               let f1 = (a * x) + (b * y) + c - (r * a2b2) in
               let f2 = (a * x) + (b * y) + c + (r * a2b2) in
               let a', b', c' = LineSegmentRay.to_abc line in
               let a2b2' = N.(sqrt (square a' + square b')) in
               let to_polynomial =
                 Formula.to_polynomial
                   ~eps
                   ~values:(Values.to_function global)
                   ~scoped_values:(function
                     | s when s = global_scope -> Values.to_function global
                     | 1 -> Values.to_function values
                     | 2 ->
                       begin
                         function
                         | `with_line `a -> a'
                         | `with_line `b -> b'
                         | `with_line `c -> c'
                         | `with_line `a2b2 -> a2b2'
                         | _ -> assert false
                       end
                     | _ -> assert false)
               in
               let abc = (a * x) + (b * y) + c |> to_polynomial in
               let is_qwf ~p1 ~p2 ~root =
                 let x0 = x |> to_polynomial |> Solver.Polynomial.calc ~x:root in
                 let y0 = y |> to_polynomial |> Solver.Polynomial.calc ~x:root in
                 let Point.{ x = x1; y = y1 } = p1 in
                 let Point.{ x = x2; y = y2 } = p2 in
                 let v1 = N.(x2 - x1, y2 - y1) in
                 let v2 = N.(x0 - x1, y0 - y1) in
                 N.(Vector.dot v1 v2 > zero)
               in
               let c f ~cond =
                 f
                 |> to_polynomial
                 |> Solver.PolynomialEquation.roots ~eps
                 |> List.filter ~f:(fun root ->
                        is_in_interval root i
                        && cond (Solver.Polynomial.calc abc ~x:root)
                        &&
                        match line.kind with
                        | `Line -> true
                        | `Ray -> is_qwf ~p1:line.p1 ~p2:line.p2 ~root
                        | `Segment ->
                          is_qwf ~p1:line.p1 ~p2:line.p2 ~root
                          && is_qwf ~p1:line.p2 ~p2:line.p1 ~root)
               in
               c f1 ~cond:N.(fun a -> a >= zero) @ c f2 ~cond:N.(fun a -> a < zero)
               |> List.min_elt ~compare:N.compare)
        |> Sequence.hd
      ;;

      let first_collision ~global bodies ~lines ~r =
        Sequence.cartesian_product bodies lines
        |> Sequence.filter_map ~f:(fun ((id, (body : Figure2.t)), line) ->
               let distance = distance_beetween_body_and_line ~body ~line in
               let radius = Values.get_scalar_exn body.values ~var:`r in
               if N.(radius + eps < distance)
               then (
                 let%map.Option ret =
                   collision ~rules:body.rules ~line ~r ~values:body.values ~global ~eps
                 in
                 ret, id, line)
               else None)
        |> Sequence.min_elt ~compare:(fun (a, _, _) (b, _, _) -> N.compare a b)
      ;;
    end
  end

  module CollisionHandle = struct
    let collision ~v1 ~v2 ~x1 ~x2 ~m1 ~m2 =
      let module V = Vector in
      let two = N.(one + one) in
      let q1 = v1 in
      let q2 =
        N.(
          match m1 = infinity, m2 = infinity with
          | true, true -> one
          | true, false -> zero
          | false, true -> two
          | false, false -> two * m2 / (m1 + m2))
      in
      let q3 = N.(V.(dot (v1 - v2) (x1 - x2)) / square V.(len (x2 - x1))) in
      let q4 = V.(x1 - x2) in
      V.(q1 - (N.(q2 * q3) ^* q4))
    ;;

    let collision_body ~v1 ~v2 ~m1 ~m2 ~x1 ~y1 ~x2 ~y2 =
      let v1' = collision ~v1 ~v2 ~x1:(x1, y1) ~x2:(x2, y2) ~m1 ~m2 in
      let v2' = collision ~v1:v2 ~v2:v1 ~x1:(x2, y2) ~x2:(x1, y1) ~m1:m2 ~m2:m1 in
      if N.(is_finite m1 && is_finite m2)
         && not
              N.(
                abs Vector.(len ((v1 *^ m1) + (v2 *^ m2) - (v1' *^ m1) - (v2' *^ m2)))
                < eps)
      then
        Error.raise_s
          [%message
            "collision_body"
              ~v1:(v1 : N.t * N.t)
              ~v2:(v2 : N.t * N.t)
              ~m1:(m1 : N.t)
              ~m2:(m2 : N.t)
              ~x1:(x1 : N.t)
              ~y1:(y1 : N.t)
              ~x2:(x2 : N.t)
              ~y2:(y2 : N.t)
              ~eps:(eps : N.t)
              ~v1':(v1' : N.t * N.t)
              ~v2':(v2' : N.t * N.t)]
      else v1', v2'
    ;;

    let calculate_new_v values1 values2 =
      let v1 = Values.get_vector_exn values1 ~var_x:`v0_x ~var_y:`v0_y in
      let v2 = Values.get_vector_exn values2 ~var_x:`v0_x ~var_y:`v0_y in
      let m1 = Values.get_scalar_exn values1 ~var:`m in
      let m2 = Values.get_scalar_exn values2 ~var:`m in
      let x1 = Values.get_scalar_exn values1 ~var:`x0 in
      let y1 = Values.get_scalar_exn values1 ~var:`y0 in
      let x2 = Values.get_scalar_exn values2 ~var:`x0 in
      let y2 = Values.get_scalar_exn values2 ~var:`y0 in
      let v1', v2' = collision_body ~v1 ~v2 ~m1 ~m2 ~x1 ~y1 ~x2 ~y2 in
      (* TODO: inf m *)
      let v1' = if N.(m1 = infinity) then v1 else v1' in
      let v2' = if N.(m2 = infinity) then v2 else v2' in
      v1', v2'
    ;;

    let calculate_new_v_with_point ~body ~point:Point.{ x = x2; y = y2 } =
      let v1 = Values.get_vector_exn body.Figure2.values ~var_x:`v0_x ~var_y:`v0_y in
      let m1 = Values.get_scalar_exn body.values ~var:`m in
      let x1 = Values.get_scalar_exn body.values ~var:`x0 in
      let y1 = Values.get_scalar_exn body.values ~var:`y0 in
      fst @@ collision_body ~v1 ~v2:N.(zero, zero) ~m1 ~m2:N.infinity ~x1 ~y1 ~x2 ~y2
    ;;

    let calculate_new_v_with_line ~body ~line =
      let a, b, c = LineSegmentRay.to_abc line in
      let v1 = Values.get_vector_exn body.Figure2.values ~var_x:`v0_x ~var_y:`v0_y in
      let m1 = Values.get_scalar_exn body.values ~var:`m in
      let x1 = Values.get_scalar_exn body.values ~var:`x0 in
      let y1 = Values.get_scalar_exn body.values ~var:`y0 in
      let v2 = N.(zero, zero) in
      let m2 = N.infinity in
      let x2 = N.(x1 - (a * c / (square a + square b))) in
      let y2 = N.(y1 - (b * c / (square a + square b))) in
      fst @@ collision_body ~v1 ~v2 ~m1 ~m2 ~x1 ~y1 ~x2 ~y2
    ;;
  end

  module Scene = struct
    module Cause = struct
      type t =
        | Init
        | Collision of
            { id1 : Figure2.Id.t
            ; id2 : Figure2.Id.t
            }
        | CollisionWithPoint of
            { id : Figure2.Id.t
            ; point : Point.t
            }
        | CollisionWithLine of
            { id : Figure2.Id.t
            ; line : LineSegmentRay.t
            }
        | VelocityGiven of
            { id : Figure2.Id.t
            ; v : N.t * N.t
            }
        | BodyAdded of { id : Figure2.Id.t }
        | PointAdded of Point.t
        | LineAdded of LineSegmentRay.t
        | Empty
      [@@deriving sexp, equal, compare]
    end

    module Figures : sig
      type t [@@deriving sexp, equal]

      val calc : t -> t:N.t -> global_values:Values.t -> t
      val add : t -> id:Figure2.Id.t -> body:Figure2.t -> t
      val empty : t
      val to_sequence : t -> (Figure2.Id.t * Figure2.t) Sequence.t
      val get_by_id : t -> id:Figure2.Id.t -> Figure2.t
      val update_by_id : t -> id:Figure2.Id.t -> body:Figure2.t -> t

      module Diff :
        Utils.AdvancedMapDiff
          with type t = t
           and type key = Figure2.Id.t
           and type value = Figure2.t
    end = struct
      include Utils.MakeAdvancedMap (Figure2.Id) (Figure2)

      let calc figures ~t ~global_values =
        figures
        |> Map.map ~f:(fun f ->
               Figure2.calc
                 ~values:(Values.to_function f.Figure2.values)
                 ~rules:f.rules
                 ~scoped_values:(Values.global_to_scoped global_values)
                 ~t
               |> Option.map ~f:(fun (xy, rules) -> Figure2.update_x0y0 ~body:f xy ~rules)
               |> Option.value ~default:f)
      ;;
    end

    type t =
      { time : N.t
      ; bodies : Figures.t
      ; points : Points.t
      ; lines : Lines.t
      ; global_values : Values.t
      ; cause : Cause.t list
      }
    [@@deriving sexp, equal]

    let update { global_values; _ } ~bodies ~points ~lines ~cause ~time =
      { time; bodies; points; lines; global_values; cause }
    ;;

    let add_figure figures ~id ~x0 ~y0 ~r ~mu ~m =
      Figures.add
        figures
        ~id
        ~body:
          Figure2.
            { id
            ; values =
                Values.of_alist
                  [ `x0, x0
                  ; `y0, y0
                  ; `v0_x, N.zero
                  ; `v0_y, N.zero
                  ; `r, r
                  ; `mu, mu
                  ; `m, m
                  ]
            ; rules = Rule.rules0
            }
    ;;

    let init ~g =
      { time = N.zero
      ; bodies = Figures.empty
      ; points = Points.empty
      ; lines = Lines.empty
      ; global_values = Values.of_alist [ `g, g ]
      ; cause = [ Init ]
      }
    ;;

    module Diff = struct
      type diff =
        { new_time : N.t
        ; bodies_diff : Figures.Diff.diff
        ; points_diff : Points.Diff.diff
        ; lines_diff : Lines.Diff.diff
        ; global_values_diff : Values.Diff.diff
        ; new_cause : Cause.t list
        }
      [@@deriving sexp, equal]

      let diff ~old curr =
        let new_time = curr.time in
        let bodies_diff = Figures.Diff.diff ~old:old.bodies curr.bodies in
        let points_diff = Points.Diff.diff ~old:old.points curr.points in
        let lines_diff = Lines.Diff.diff ~old:old.lines curr.lines in
        let global_values_diff =
          Values.Diff.diff ~old:old.global_values curr.global_values
        in
        let new_cause = curr.cause in
        { new_time; bodies_diff; points_diff; lines_diff; global_values_diff; new_cause }
      ;;

      let apply_diff ~diff old =
        { time = diff.new_time
        ; bodies = Figures.Diff.apply_diff ~diff:diff.bodies_diff old.bodies
        ; points = Points.Diff.apply_diff ~diff:diff.points_diff old.points
        ; lines = Lines.Diff.apply_diff ~diff:diff.lines_diff old.lines
        ; global_values =
            Values.Diff.apply_diff ~diff:diff.global_values_diff old.global_values
        ; cause = diff.new_cause
        }
      ;;
    end
  end

  module Action = struct
    type a =
      | AddBody of
          { id : Figure2.Id.t
          ; x0 : N.t
          ; y0 : N.t
          ; r : N.t
          ; mu : N.t
          ; m : N.t
          }
      | AddPoint of Point.t
      | AddLine of LineSegmentRay.t
      | GiveVelocity of
          { id : Figure2.Id.t
          ; v0 : N.t * N.t
          }
      | Empty
    [@@deriving sexp, equal]

    type t =
      { time : N.t
      ; action : a
      ; timeout : N.t option
      }
    [@@deriving sexp, equal]
  end

  module Model : sig
    module Scenes : sig
      type t [@@deriving sexp, equal]

      val before : t -> time:N.t -> t * Scene.t
      val merge_with_list : t -> Scene.t list -> t
      val last_exn : t -> Scene.t
    end

    type t =
      { scenes : Scenes.t
      ; timeout : N.t option
      }
    [@@deriving sexp, equal]

    val init : g:N.t -> t
    val of_scenes : Scenes.t -> time:N.t -> scene:Scene.t -> timeout:N.t option -> t

    module Diff : sig
      type diff =
        { init : [ `Init of Scene.t | `Since of N.t ]
        ; scene_diffs : Scene.Diff.diff list
        ; new_timeout : N.t option
        }

      include Utils.Diff with type t := t and type diff := diff
    end
  end = struct
    module Scenes = struct
      include Utils.MakeAdvancedMap (N) (Scene)

      let before scenes ~time =
        match Map.split scenes time with
        | l, Some (k, v), _ -> Map.add_exn l ~key:k ~data:v, v
        | l, None, _ -> l, snd @@ Map.max_elt_exn l
      ;;

      let merge_with_list scenes l =
        (* TODO *)
        let l =
          Map.of_alist_reduce
            (module N)
            (List.map l ~f:(fun scene -> scene.Scene.time, scene))
            ~f:(fun _a b -> b)
        in
        Map.merge_skewed scenes l ~combine:(fun ~key v1 v2 ->
            Scene.update
              v2
              ~bodies:v2.bodies
              ~cause:
                (* TODO *)
                (v2.cause @ v1.cause |> List.dedup_and_sort ~compare:Scene.Cause.compare)
              ~points:v2.points
              ~lines:v2.lines
              ~time:key)
      ;;

      let last_exn = Common.Fn.(Map.max_elt_exn >> snd)
    end

    type t =
      { scenes : Scenes.t
      ; timeout : N.t option
      }
    [@@deriving sexp, equal]

    let init ~g =
      { scenes = Map.of_alist_exn (module N) [ N.zero, Scene.init ~g ]; timeout = None }
    ;;

    let of_scenes scenes ~time ~scene ~timeout =
      { scenes =
          Map.update scenes time ~f:(function
              | Some s ->
                Scene.update
                  s
                  ~bodies:scene.Scene.bodies
                  ~cause:(scene.cause @ s.cause)
                  ~points:scene.points
                  ~lines:scene.lines
                  ~time
              | None -> scene)
      ; timeout
      }
    ;;

    module Diff = struct
      type diff =
        { init : [ `Init of Scene.t | `Since of N.t ]
        ; scene_diffs : Scene.Diff.diff list
        ; new_timeout : N.t option
        }
      [@@deriving sexp, equal]

      let diff ~old curr =
        let old_keys = Map.keys (Scenes.to_map old.scenes) |> Sequence.of_list in
        let new_keys = Map.keys (Scenes.to_map curr.scenes) |> Sequence.of_list in
        let zipped = Sequence.zip_full old_keys new_keys in
        let init =
          Sequence.fold_until
            zipped
            ~init:None
            ~f:
              (fun acc -> function
                | `Both (a, b) when N.(a = b) ->
                  let av = Scenes.get_by_id ~id:a old.scenes in
                  let bv = Scenes.get_by_id ~id:b curr.scenes in
                  if Scene.equal av bv then Continue (Some a) else Stop acc
                | _ -> Stop acc)
            ~finish:(fun acc -> acc)
          |> Option.map ~f:(fun since -> `Since since)
          |> Option.value
               ~default:(`Init (Scenes.of_map curr.scenes |> Map.min_elt_exn |> snd))
        in
        let init_time, init_scene =
          match init with
          | `Init scene -> scene.time, scene
          | `Since since -> since, Map.find_exn (Scenes.to_map curr.scenes) since
        in
        let scene_diffs =
          Scenes.to_map curr.scenes
          |> Map.filter_keys ~f:N.(fun k -> k > init_time)
          |> Map.to_sequence
          |> Sequence.folding_map ~init:init_scene ~f:(fun prev (_, curr) ->
                 curr, Scene.Diff.diff ~old:prev curr)
          |> Sequence.to_list
        in
        { init; scene_diffs; new_timeout = curr.timeout }
      ;;

      let apply_diff ~diff old =
        let common_scenes, init_scene =
          match diff.init with
          | `Init init -> Map.singleton (module N) init.time init, init
          | `Since since -> Scenes.before (Scenes.to_map old.scenes) ~time:since
        in
        let scenes =
          diff.scene_diffs
          |> Sequence.of_list
          |> Sequence.folding_map ~init:init_scene ~f:(fun prev diff ->
                 let ret = Scene.Diff.apply_diff ~diff prev in
                 ret, (ret.time, ret))
          |> Map.of_sequence_exn (module N)
          |> Map.merge_skewed common_scenes ~combine:(fun ~key:_ l _ -> l)
          |> Scenes.of_map
        in
        { scenes; timeout = diff.new_timeout }
      ;;
    end
  end

  module Engine = struct
    let forward ?time (scene : Scene.t) ~timeout =
      let rec forward_rec acc =
        match acc, timeout with
        | Scene.{ time; _ } :: tl, Some timeout when N.(time > timeout) -> tl
        | [], _ -> []
        | scene :: _, _ ->
          let with_body =
            Scene.Figures.to_sequence scene.bodies
            |> CollisionDetection.WithBody.first_collision
                 ~global:scene.global_values
                 ~r:(Formula.of_alist_exn Figure2.Rule.Exprs.[ 0, r ])
                 ~a:Figure2.Rule.Exprs.a_vec
          in
          let with_point =
            CollisionDetection.WithPoint.first_collision
              ~global:scene.global_values
              ~points:(Points.to_sequence scene.points)
              ~r:(Formula.of_alist_exn Figure2.Rule.Exprs.[ 0, r ])
              (Scene.Figures.to_sequence scene.bodies)
          in
          let with_line =
            CollisionDetection.WithLine.first_collision
              ~global:scene.global_values
              ~lines:(Lines.to_sequence scene.lines)
              ~r:(Formula.of_alist_exn Figure2.Rule.Exprs.[ 0, r ])
              (Scene.Figures.to_sequence scene.bodies)
          in
          let coll t ~id1 ~id2 =
            let q =
              Scene.Figures.calc scene.bodies ~t ~global_values:scene.global_values
            in
            let body1 = Scene.Figures.get_by_id q ~id:id1 in
            let body2 = Scene.Figures.get_by_id q ~id:id2 in
            let v1n, v2n = CollisionHandle.calculate_new_v body1.values body2.values in
            Scene.update
              scene
              ~bodies:
                (q
                |> Scene.Figures.update_by_id
                     ~id:id1
                     ~body:(Figure2.update_v0 body1 ~v:v1n ~rules:Figure2.Rule.rules1)
                |> Scene.Figures.update_by_id
                     ~id:id2
                     ~body:(Figure2.update_v0 body2 ~v:v2n ~rules:Figure2.Rule.rules1))
              ~cause:[ Collision { id1; id2 } ]
              ~time:N.(scene.time + t)
              ~points:scene.points
              ~lines:scene.lines
          in
          let coll_with_point (t, id, point) =
            let q =
              Scene.Figures.calc scene.bodies ~t ~global_values:scene.global_values
            in
            let body = Scene.Figures.get_by_id q ~id in
            let v' = CollisionHandle.calculate_new_v_with_point ~body ~point in
            let q =
              Scene.Figures.update_by_id
                q
                ~id
                ~body:(Figure2.update_v0 body ~v:v' ~rules:Figure2.Rule.rules1)
            in
            let new_time = N.(scene.time + t) in
            Scene.update
              scene
              ~bodies:q
              ~cause:[ CollisionWithPoint { id; point } ]
              ~time:new_time
              ~points:scene.points
              ~lines:scene.lines
          in
          let coll_with_line (t, id, line) =
            let q =
              Scene.Figures.calc scene.bodies ~t ~global_values:scene.global_values
            in
            let body = Scene.Figures.get_by_id q ~id in
            let v' = CollisionHandle.calculate_new_v_with_line ~body ~line in
            let q =
              Scene.Figures.update_by_id
                q
                ~id
                ~body:(Figure2.update_v0 body ~v:v' ~rules:Figure2.Rule.rules1)
            in
            let new_time = N.(scene.time + t) in
            Scene.update
              scene
              ~bodies:q
              ~cause:[ CollisionWithLine { id; line } ]
              ~time:new_time
              ~points:scene.points
              ~lines:scene.lines
          in
          let mt =
            [ Option.map with_body ~f:(fun ((t, _, _) as a) -> t, `WithBody a)
            ; Option.map with_point ~f:(fun ((t, _, _) as a) -> t, `WithPoint a)
            ; Option.map with_line ~f:(fun ((t, _, _) as a) -> t, `WithLine a)
            ]
            |> List.filter_opt
            |> List.min_elt ~compare:(fun (t1, _) (t2, _) -> N.compare t1 t2)
            |> Option.map ~f:snd
          in
          (match mt, time with
          | Some (`WithBody (t, id1, id2)), Some time ->
            let s = coll t ~id1 ~id2 in
            if N.(s.Scene.time < time)
            then forward_rec (s :: acc)
            else
              [ Scene.update
                  scene
                  ~bodies:
                    (Scene.Figures.calc
                       scene.bodies
                       ~t:N.(time - scene.time)
                       ~global_values:scene.global_values)
                  ~cause:[]
                  ~time
                  ~points:scene.points
                  ~lines:scene.lines
              ]
          | Some (`WithBody (t, id1, id2)), None ->
            let s = coll t ~id1 ~id2 in
            forward_rec (s :: acc)
          | Some (`WithPoint r), Some time ->
            let s = coll_with_point r in
            if N.(s.Scene.time < time)
            then forward_rec (s :: acc)
            else
              [ Scene.update
                  scene
                  ~bodies:
                    (Scene.Figures.calc
                       scene.bodies
                       ~t:N.(time - scene.time)
                       ~global_values:scene.global_values)
                  ~cause:[]
                  ~time
                  ~points:scene.points
                  ~lines:scene.lines
              ]
          | Some (`WithPoint r), None ->
            let s = coll_with_point r in
            forward_rec (s :: acc)
          | Some (`WithLine r), Some time ->
            let s = coll_with_line r in
            if N.(s.Scene.time < time)
            then forward_rec (s :: acc)
            else
              [ Scene.update
                  scene
                  ~bodies:
                    (Scene.Figures.calc
                       scene.bodies
                       ~t:N.(time - scene.time)
                       ~global_values:scene.global_values)
                  ~cause:[]
                  ~time
                  ~points:scene.points
                  ~lines:scene.lines
              ]
          | Some (`WithLine r), None ->
            let s = coll_with_line r in
            forward_rec (s :: acc)
          | None, Some time ->
            let q =
              Scene.Figures.calc
                scene.bodies
                ~t:N.(time - scene.time)
                ~global_values:scene.global_values
            in
            [ Scene.update
                scene
                ~bodies:q
                ~cause:[]
                ~time
                ~points:scene.points
                ~lines:scene.lines
            ]
          | None, None -> acc)
      in
      (* TODO *)
      forward_rec [ scene ] |> List.rev
    ;;

    let recv model ~action:Action.{ time; action; timeout } =
      let before, s = Model.Scenes.before model.Model.scenes ~time in
      let scenes = forward s ~time ~timeout in
      let scenes = Model.Scenes.merge_with_list before scenes in
      let before, s = Model.Scenes.before scenes ~time in
      let r =
        match action with
        | Action.AddBody { id; x0; y0; r; mu; m } ->
          Scene.update
            s
            ~bodies:(Scene.add_figure s.bodies ~id ~x0 ~y0 ~r ~mu ~m)
            ~cause:[ BodyAdded { id } ]
            ~time
            ~points:s.points
            ~lines:s.lines
        | AddPoint point ->
          Scene.update
            s
            ~bodies:s.bodies
            ~cause:[ PointAdded point ]
            ~time
            ~points:(Points.add s.points ~el:point)
            ~lines:s.lines
        | AddLine line ->
          Scene.update
            s
            ~bodies:s.bodies
            ~cause:[ LineAdded line ]
            ~time
            ~points:s.points
            ~lines:(Lines.add s.lines ~el:line)
        | GiveVelocity { id; v0 } ->
          let body = Scene.Figures.get_by_id s.bodies ~id in
          let body = Figure2.update_v0 body ~v:v0 ~rules:Figure2.Rule.rules1 in
          Scene.update
            s
            ~bodies:(Scene.Figures.update_by_id s.bodies ~id:body.id ~body)
            ~cause:[ VelocityGiven { id; v = v0 } ]
            ~time
            ~points:s.points
            ~lines:s.lines
        | Empty ->
          Scene.update
            s
            ~bodies:s.bodies
            ~cause:[ Empty ]
            ~time
            ~points:s.points
            ~lines:s.lines
      in
      let m = before |> Model.of_scenes ~time:r.time ~scene:r ~timeout in
      let f = forward (Model.Scenes.last_exn m.scenes) ~timeout in
      { m with scenes = Model.Scenes.merge_with_list m.scenes f }
    ;;

    let update model ~action =
      match action with
      | `Action a -> recv model ~action:a
      | `Replace m -> m
      | `Diff diff -> Model.Diff.apply_diff model ~diff
    ;;

    let recv_with_diff model ~action =
      match action with
      | `Action a ->
        let updated = recv model ~action:a in
        updated, Model.Diff.diff ~old:model updated
    ;;
  end
end
