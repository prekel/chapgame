open Core

module Make (N : Module_types.Number) = struct
  module Vars :
    Identifiable.S
      with type t =
        [ `x0
        | `y0
        | `v0
        | `r
        | `g
        | `mu
        | `m
        ] = struct
    module T = struct
      type t =
        [ `x0
        | `y0
        | `v0
        | `r
        | `g
        | `mu
        | `m
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

  let global_scope : Scope.t = -1

  module Values : sig
    type t [@@deriving sexp, equal]

    val get_scalar_exn : t -> var:Vars.t -> N.t
    val get_vector_exn : t -> var:Vars.t -> N.t * N.t
    val update_scalar : t -> var:Vars.t -> value:N.t -> t
    val update_vector : t -> var:Vars.t -> value:N.t * N.t -> t
    val of_alist : (Vars.t * Expr.value) list -> t
    val to_function : t -> Expr.values
    val global_to_scoped : t -> Scope.t -> Expr.values
  end = struct
    type t = (Vars.t, Expr.value, Vars.comparator_witness) Map.t

    let equal = Map.equal Expr.equal_value
    let sexp_of_t : t -> _ = Common.Map.sexp_of_t Vars.sexp_of_t Expr.sexp_of_value

    let t_of_sexp : _ -> t =
      Common.Map.t_of_sexp Vars.t_of_sexp Expr.value_of_sexp (module Vars)
    ;;

    let get_scalar_exn values ~var = Map.find_exn values var |> Expr.scalar_exn
    let get_vector_exn values ~var = Map.find_exn values var |> Expr.vector_exn

    let update_scalar values ~var ~value =
      Map.update values var ~f:(fun _ -> Expr.Scalar value)
    ;;

    let update_vector values ~var ~value =
      Map.update values var ~f:(fun _ -> Expr.Vector value)
    ;;

    let of_alist = Map.of_alist_exn (module Vars)
    let to_function = Map.find_exn

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
            ([ `Interval of Expr.t_scalar * Expr.t_scalar
             | `PosInfinity of Expr.t_scalar
             ]
            [@sexp.opaque])
        ; x : (Formula.t[@sexp.opaque])
        ; y : (Formula.t[@sexp.opaque])
        ; v_x : (Formula.t[@sexp.opaque])
        ; v_y : (Formula.t[@sexp.opaque])
        ; after : ((t[@sexp.opaque]) list[@sexp.opaque])
        ; name : string
        }
      [@@deriving of_sexp, equal]

      let sexp_of_t { name; _ } = Sexp.Atom name
    end

    type t =
      { id : Id.t
      ; values : Values.t
      ; rules : Rule.t list
      }
    [@@deriving sexp, equal]

    let calc ~values ~rules ~scoped_values ~t ~eps =
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
          |> Values.update_vector ~var:`v0 ~value:(v_x, v_y)
      ; rules
      }
    ;;

    let update_v0 body ~v ~rules =
      { body with values = body.values |> Values.update_vector ~var:`v0 ~value:v; rules }
    ;;

    let update body ~var ~value =
      { body with values = body.values |> Values.update_vector ~var ~value }
    ;;
  end

  module CollisionDetection = struct
    let b2 ~rules1 ~rules2 ~r =
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
             a.interval, b.interval, f)
    ;;

    let b3 p ~values1 ~values2 ~global ~eps =
      let inter values =
        let calc v =
          let r =
            Expr.calc
              ~values
              ~scoped_values:(function
                | s when s = global_scope -> global
                | _ -> assert false)
              (module N)
              v
          in
          assert (N.is_finite r);
          r
        in
        function
        | `Interval (l, r) -> `Interval (calc l, calc r)
        | `PosInfinity l -> `PosInfinity (calc l)
      in
      Sequence.map p ~f:(fun (ai, bi, f) ->
          ( inter values1 ai
          , inter values2 bi
          , Formula.to_polynomial f ~eps ~values:global ~scoped_values:(function
                | s when s = global_scope -> global
                | 1 -> values1
                | 2 -> values2
                | _ -> assert false) ))
    ;;

    let b4 p ~eps =
      let is_in_interval n = function
        | `Interval (l, r) -> N.(l <= n && n < r)
        | `PosInfinity l -> N.(l <= n)
      in
      Sequence.filter_map p ~f:(fun (ai, bi, p) ->
          let roots = Solver.PolynomialEquation.roots p ~eps in
          match
            roots
            |> List.filter ~f:(fun root ->
                   is_in_interval root ai && is_in_interval root bi)
          with
          | [] -> None
          | l -> Some (Sequence.of_list l))
      |> Sequence.concat
    ;;

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
      let inter values =
        let calc v =
          Expr.calc
            ~values:(Values.to_function values)
            ~scoped_values:(function
              | s when s = global_scope -> Values.to_function global
              | _ -> assert false)
            (module N)
            v
        in
        function
        | `Interval (l, r) -> `Interval (calc l, calc r)
        | `PosInfinity l -> `PosInfinity (calc l)
      in
      let is_in_interval n = function
        | `Interval (l, r) -> N.(l <= n && n < r)
        | `PosInfinity l -> N.(l <= n)
      in
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
             let ai = inter values1 a.interval in
             let bi = inter values2 b.interval in
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

    let distance ~x1 ~y1 ~x2 ~y2 =
      let sqr a = N.(a * a) in
      N.(sqrt (sqr (x2 - x1) + sqr (y2 - y1)))
    ;;

    let distance_bentween_bodies values1 values2 =
      let x1 = Values.get_scalar_exn values1 ~var:`x0 in
      let y1 = Values.get_scalar_exn values1 ~var:`y0 in
      let x2 = Values.get_scalar_exn values2 ~var:`x0 in
      let y2 = Values.get_scalar_exn values2 ~var:`y0 in
      distance ~x1 ~y1 ~x2 ~y2
    ;;

    let collisions ~eps ~global_values (bodies : (_ * Figure2.t) Sequence.t) ~r =
      Sequence.cartesian_product bodies bodies
      |> Sequence.filter_map ~f:(fun ((id1, f1), (id2, f2)) ->
             let r1 = Values.get_scalar_exn f1.values ~var:`r in
             let r2 = Values.get_scalar_exn f2.values ~var:`r in
             let distance = distance_bentween_bodies f1.values f2.values in
             if N.(r1 + r2 + eps < distance)
             then
               Some
                 ( id1
                 , id2
                 , let p2 = b2 ~rules1:f1.rules ~rules2:f2.rules ~r in
                   let p3 =
                     b3
                       p2
                       ~values1:(Values.to_function f1.values)
                       ~values2:(Values.to_function f2.values)
                       ~global:global_values
                       ~eps
                   in
                   let p4 = b4 p3 ~eps in
                   p4 )
             else None)
    ;;

    let collisions_extra ~eps ~global_values (bodies : (_ * Figure2.t) Sequence.t) ~r ~a =
      Sequence.cartesian_product bodies bodies
      |> Sequence.filter_map ~f:(fun ((id1, f1), (id2, f2)) ->
             let r1 = Values.get_scalar_exn f1.values ~var:`r in
             let r2 = Values.get_scalar_exn f2.values ~var:`r in
             let distance = distance_bentween_bodies f1.values f2.values in
             if N.(r1 + r2 + eps < distance)
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
  end

  module CollisionHandle = struct
    module Vector : sig
      include module type of Expr.VectorOps

      val dot : t -> t -> N.t
      val len : t -> N.t
      val ( *^ ) : t -> N.t -> t
      val ( ^* ) : N.t -> t -> t
    end = struct
      include Expr.VectorOps

      let dot a b =
        let x, y = a * b in
        N.(x + y)
      ;;

      let len (x, y) = N.(sqrt ((x * x) + (y * y)))
      let ( *^ ) (a, b) c = N.(a * c, b * c)
      let ( ^* ) c (a, b) = N.(a * c, b * c)
    end

    let collision ~v1 ~v2 ~x1 ~x2 ~m1 ~m2 =
      let module V = Vector in
      let two = N.(one + one) in
      let sqr a = N.(a * a) in
      let q1 = v1 in
      let q2 = N.(two * m2 / (m1 + m2)) in
      let q3 = N.(V.(dot (v1 - v2) (x1 - x2)) / sqr V.(len (x2 - x1))) in
      let q4 = V.(x1 - x2) in
      V.(q1 - (N.(q2 * q3) ^* q4))
    ;;

    let collision_body ~v1 ~v2 ~m1 ~m2 ~x1 ~y1 ~x2 ~y2 ~eps =
      let _ = eps in
      let v1' = collision ~v1 ~v2 ~x1:(x1, y1) ~x2:(x2, y2) ~m1 ~m2 in
      let v2' = collision ~v1:v2 ~v2:v1 ~x1:(x2, y2) ~x2:(x1, y1) ~m1:m2 ~m2:m1 in
      if not
           N.(
             abs Vector.(len ((v1 *^ m1) + (v2 *^ m2) - (v1' *^ m1) - (v2' *^ m2))) < eps)
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

    let calculate_new_v values1 values2 ~eps =
      let v1 = Values.get_vector_exn values1 ~var:`v0 in
      let v2 = Values.get_vector_exn values2 ~var:`v0 in
      let m1 = Values.get_scalar_exn values1 ~var:`m in
      let m2 = Values.get_scalar_exn values2 ~var:`m in
      let x1 = Values.get_scalar_exn values1 ~var:`x0 in
      let y1 = Values.get_scalar_exn values1 ~var:`y0 in
      let x2 = Values.get_scalar_exn values2 ~var:`x0 in
      let y2 = Values.get_scalar_exn values2 ~var:`y0 in
      collision_body ~v1 ~v2 ~m1 ~m2 ~x1 ~y1 ~x2 ~y2 ~eps
    ;;
  end

  module Exprs = struct
    open Expr.Syntax

    let g = scope ~scope:global_scope (fst @@ scalar_var `g)
    let mu, _ = scalar_var `mu
    let v0_vec, v0_vec_name = vector_var `v0

    let a_vec =
      let f = mu * g in
      -vector_unit v0_vec * vector_of_scalar f f
    ;;

    let a_x = vector_x a_vec
    let a_y = vector_y a_vec
    let v0_x = vector_x v0_vec
    let v0_y = vector_y v0_vec
    let x0, x0_name = scalar_var `x0
    let y0, y0_name = scalar_var `y0
    let r, r_name = scalar_var `r
    let half = scalar_const N.(one / (one + one))
    let zero = Expr.ScalarZero
    let border_l1 = zero
    let border_r1 = vector_length v0_vec / vector_length a_vec
    let border_l2 = border_r1
    let three = scalar_const N.(one + one + one)
    let two = scalar_const N.(one + one)
    let pi = scalar_const N.pi
  end

  module Formulas = struct
    open Exprs
    open Expr.Syntax

    let x = Formula.of_alist_exn [ 0, x0; 1, v0_x; 2, a_x / two ]
    let y = Formula.of_alist_exn [ 0, y0; 1, v0_y; 2, a_y / two ]
    let v_x = Formula.of_alist_exn [ 0, v0_x; 1, a_x ]
    let v_y = Formula.of_alist_exn [ 0, v0_y; 1, a_y ]

    let x_after =
      Formula.of_alist_exn
        [ ( 0
          , x0
            + (v0_x * vector_length v0_vec / vector_length a_vec)
            + (a_x * sqr (vector_length v0_vec) / (two * sqr (vector_length a_vec))) )
        ]
    ;;

    let y_after =
      Formula.of_alist_exn
        [ ( 0
          , y0
            + (v0_y * vector_length v0_vec / vector_length a_vec)
            + (a_y * sqr (vector_length v0_vec) / (two * sqr (vector_length a_vec))) )
        ]
    ;;

    let x0 = Formula.of_alist_exn [ 0, x0 ]
    let y0 = Formula.of_alist_exn [ 0, y0 ]
    let v_x_after = Formula.of_alist_exn [ 0, zero ]
    let v_y_after = Formula.of_alist_exn [ 0, zero ]
    let r = Formula.of_alist_exn [ 0, r ]
  end

  module Scene = struct
    module Cause = struct
      type t =
        | Init
        | Collision of
            { id1 : Figure2.Id.t
            ; id2 : Figure2.Id.t
            ; xy1 : N.t * N.t
            ; xy2 : N.t * N.t
            ; v1_before : N.t * N.t
            ; v2_before : N.t * N.t
            ; v1 : N.t * N.t
            ; v2 : N.t * N.t
            ; extra : CollisionDetection.extra
            }
        | VelocityGiven of
            { id : Figure2.Id.t
            ; v : N.t * N.t
            }
        | BodyAdded of { id : Figure2.Id.t }
        | Empty
      [@@deriving sexp, equal]
    end

    module Figures : sig
      type t [@@deriving sexp, equal]

      val calc : t -> t:N.t -> global_values:Values.t -> eps:N.t -> t
      val add : t -> id:Figure2.Id.t -> body:Figure2.t -> t
      val empty : t
      val to_sequence : t -> (Figure2.Id.t * Figure2.t) Sequence.t
      val get_by_id : t -> id:Figure2.Id.t -> Figure2.t
      val update_by_id : t -> id:Figure2.Id.t -> body:Figure2.t -> t
      val to_map : t -> (Figure2.Id.t, Figure2.t, Figure2.Id.comparator_witness) Map.t
    end = struct
      type t = (Figure2.Id.t, Figure2.t, Figure2.Id.comparator_witness) Map.t

      let equal = Map.equal Figure2.equal
      let sexp_of_t = Common.Map.sexp_of_t Figure2.Id.sexp_of_t Figure2.sexp_of_t

      let t_of_sexp =
        Common.Map.t_of_sexp Figure2.Id.t_of_sexp Figure2.t_of_sexp (module Figure2.Id)
      ;;

      let calc (figures : t) ~t ~global_values ~eps =
        Map.map figures ~f:(fun f ->
            Figure2.calc
              ~values:(Values.to_function f.values)
              ~rules:f.rules
              ~scoped_values:(Values.global_to_scoped global_values)
              ~t
              ~eps
            |> Option.map ~f:(fun (xy, rules) -> Figure2.update_x0y0 ~body:f xy ~rules)
            |> Option.value ~default:f)
      ;;

      let empty = Map.empty (module Figure2.Id)
      let add t ~id ~body = Map.add_exn t ~key:id ~data:body
      let to_sequence t = Map.to_sequence t
      let get_by_id t ~id = Map.find_exn t id
      let update_by_id t ~id ~body = Map.update t id ~f:(fun _ -> body)
      let to_map = Fn.id
    end

    type t =
      { time : N.t
      ; figures : Figures.t
      ; global_values : Values.t
      ; cause : Cause.t list
      }
    [@@deriving sexp, equal]

    let update { global_values; _ } ~bodies ~cause ~time =
      { time; figures = bodies; global_values; cause }
    ;;

    let rec rules1 =
      Figure2.Rule.
        [ { interval = `Interval (Exprs.border_l1, Exprs.border_r1)
          ; x = Formulas.x
          ; y = Formulas.y
          ; v_x = Formulas.v_x
          ; v_y = Formulas.v_y
          ; after = rules1
          ; name = "rules1 - 0"
          }
        ; { interval = `PosInfinity Exprs.border_l2
          ; x = Formulas.x_after
          ; y = Formulas.y_after
          ; v_x = Formulas.v_x_after
          ; v_y = Formulas.v_y_after
          ; after = rules0
          ; name = "rules1 - 1"
          }
        ]

    and rules0 =
      [ { interval = `PosInfinity Exprs.border_l1
        ; x = Formulas.x0
        ; y = Formulas.y0
        ; v_x = Formulas.v_x_after
        ; v_y = Formulas.v_y_after
        ; after = rules0
        ; name = "rules0 - 0"
        }
      ]
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
                  [ `x0, Expr.Scalar x0
                  ; `y0, Expr.Scalar y0
                  ; `v0, Expr.Vector (N.zero, N.zero)
                  ; `r, Expr.Scalar r
                  ; `mu, Expr.Scalar mu
                  ; `m, Expr.Scalar m
                  ]
            ; rules = rules0
            }
    ;;

    let init ~g =
      { time = N.zero
      ; figures = Figures.empty
      ; global_values = Values.of_alist [ `g, Expr.Scalar g ]
      ; cause = [ Init ]
      }
    ;;
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
      | GiveVelocity of
          { id : Figure2.Id.t
          ; v0 : N.t * N.t
          }
      | Empty
    [@@deriving sexp, equal]

    type t =
      { time : N.t
      ; action : a
      }
    [@@deriving sexp]
  end

  module Model : sig
    type t [@@deriving sexp, equal]

    val empty : g:N.t -> t
    val update : t -> time:N.t -> scene:Scene.t -> t
    val before : t -> time:N.t -> t * Scene.t
    val merge_with_list : t -> Scene.t list -> t
    val last_exn : t -> Scene.t
  end = struct
    type t = (N.t, Scene.t, N.comparator_witness) Map.t

    let equal = Map.equal Scene.equal
    let sexp_of_t = Common.Map.sexp_of_t N.sexp_of_t Scene.sexp_of_t
    let t_of_sexp = Common.Map.t_of_sexp N.t_of_sexp Scene.t_of_sexp (module N)
    let empty ~g = Map.of_alist_exn (module N) [ N.zero, Scene.init ~g ]

    let update model ~time ~scene =
      Map.update model time ~f:(function
          | Some s ->
            Scene.update
              s
              ~bodies:scene.Scene.figures
              ~cause:(scene.cause @ s.cause)
              ~time
          | None -> scene)
    ;;

    let before model ~time =
      match Map.split model time with
      | l, Some (k, v), _ -> Map.add_exn l ~key:k ~data:v, v
      | l, None, _ -> l, snd @@ Map.max_elt_exn l
    ;;

    let merge_with_list model l =
      (* TODO *)
      let l =
        Map.of_alist_reduce
          (module N)
          (List.map l ~f:(fun scene -> scene.Scene.time, scene))
          ~f:(fun _a b -> b)
      in
      Map.merge_skewed model l ~combine:(fun ~key v1 v2 ->
          Scene.update v2 ~bodies:v2.figures ~cause:(v2.cause @ v1.cause) ~time:key)
    ;;

    let last_exn = Common.Fn.(Map.max_elt_exn >> snd)
  end

  module Engine = struct
    let forward ?time (scene : Scene.t) ~eps =
      let rec forward_rec (scene : Scene.t) =
        let qt =
          Scene.Figures.to_sequence scene.figures
          |> CollisionDetection.collisions_extra
               ~eps
               ~global_values:scene.global_values
               ~r:Formulas.r
               ~a:Exprs.a_vec
          |> Sequence.filter_map ~f:(fun a ->
                 match
                   List.min_elt a.CollisionDetection.roots_filtered ~compare:N.compare
                 with
                 | None -> None
                 | Some r -> Some (a, r))
          |> Sequence.min_elt ~compare:(fun (_r1, a) (_r2, b) -> N.compare a b)
        in
        let coll r t ~id1 ~id2 =
          let q =
            Scene.Figures.calc scene.figures ~t ~global_values:scene.global_values ~eps
          in
          let body1 = Scene.Figures.get_by_id q ~id:id1 in
          let body2 = Scene.Figures.get_by_id q ~id:id2 in
          let v1n, v2n = CollisionHandle.calculate_new_v body1.values body2.values ~eps in
          let q =
            Scene.Figures.update_by_id
              q
              ~id:id1
              ~body:(Figure2.update_v0 body1 ~v:v1n ~rules:Scene.rules1)
            |> Scene.Figures.update_by_id
                 ~id:id2
                 ~body:(Figure2.update_v0 body2 ~v:v2n ~rules:Scene.rules1)
          in
          let new_time = N.(scene.time + t) in
          let s =
            let body1 = Scene.Figures.get_by_id scene.figures ~id:id1 in
            let body2 = Scene.Figures.get_by_id scene.figures ~id:id2 in
            Scene.update
              scene
              ~bodies:q
              ~cause:
                [ Collision
                    { id1
                    ; id2
                    ; xy1 =
                        ( Values.get_scalar_exn body1.values ~var:`x0
                        , Values.get_scalar_exn body1.values ~var:`y0 )
                    ; xy2 =
                        ( Values.get_scalar_exn body2.values ~var:`x0
                        , Values.get_scalar_exn body2.values ~var:`y0 )
                    ; v1_before = Values.get_vector_exn body1.values ~var:`v0
                    ; v2_before = Values.get_vector_exn body2.values ~var:`v0
                    ; v1 = v1n
                    ; v2 = v2n
                    ; extra = r
                    }
                ]
              ~time:new_time
          in
          s
        in
        match qt, time with
        | Some (({ id1; id2; _ } as r), t), Some time ->
          let s = coll r t ~id1 ~id2 in
          if N.(s.time < time)
          then s :: forward_rec s
          else (
            let q =
              Scene.Figures.calc
                scene.figures
                ~t:N.(time - scene.time)
                ~global_values:scene.global_values
                ~eps
            in
            [ Scene.update scene ~bodies:q ~cause:[] ~time ])
        | Some (({ id1; id2; _ } as r), t), None ->
          let s = coll r t ~id1 ~id2 in
          s :: forward_rec s
        | _, Some time ->
          let q =
            Scene.Figures.calc
              scene.figures
              ~t:N.(time - scene.time)
              ~global_values:scene.global_values
              ~eps
          in
          [ Scene.update scene ~bodies:q ~cause:[] ~time ]
        | _, None -> []
      in
      forward_rec scene |> List.rev
    ;;

    let recv (model : Model.t) ~action:(Action.{ time; action } as _a) ~eps =
      let before, s = Model.before model ~time in
      let scenes = forward s ~time ~eps in
      let model = Model.merge_with_list before scenes in
      let before, s = Model.before model ~time in
      let r =
        match action with
        | Action.AddBody { id; x0; y0; r; mu; m } ->
          Scene.update
            s
            ~bodies:(Scene.add_figure s.figures ~id ~x0 ~y0 ~r ~mu ~m)
            ~cause:[ BodyAdded { id } ]
            ~time
        | GiveVelocity { id; v0 } ->
          let body = Scene.Figures.get_by_id s.figures ~id in
          let body = Figure2.update_v0 body ~v:v0 ~rules:Scene.rules1 in
          Scene.update
            s
            ~bodies:(Scene.Figures.update_by_id s.figures ~id:body.id ~body)
            ~cause:[ VelocityGiven { id; v = v0 } ]
            ~time
        | Empty -> s
      in
      let m = before |> Model.update ~time:r.time ~scene:r in
      let f = forward (Model.last_exn m) ~eps in
      Model.merge_with_list m f
    ;;
  end
end
