open Core

module Make (N : Module_types.Number) = struct
  module Solver = Solver.MakeSolver (N)

  module Figure = struct
    module Vec = struct
      type _ t
    end

    module Kg = struct
      type t
    end

    module Metre = struct
      type t
    end

    module Velocity = struct
      type t = M of float * float
    end

    module Newton = struct
      type t
    end

    module Force = struct
      type t =
        | Const of Newton.t Vec.t
        | Friction of float
        | No
    end
  end

  module Vars :
    Identifiable.S
      with type t =
        [ `x0
        | `y0
        | `v0
        | `r
        | `g
        | `mu
        ] = struct
    module T = struct
      type t =
        [ `x0
        | `y0
        | `v0
        | `r
        | `g
        | `mu
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

  module Expr = Expr.Make (Vars) (Int) (N)
  module Formula = Formula.Make (Vars) (Int) (N) (Expr) (Solver)

  type values = (Vars.t, Expr.value, Vars.comparator_witness) Map.t

  let sexp_of_values values = [%sexp (Map.to_alist values : (Vars.t * Expr.value) list)]

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

    type formula =
      { interval :
          [ `Interval of Expr.t_scalar * Expr.t_scalar | `PosInfinity of Expr.t_scalar ]
      ; x : Formula.t
      ; y : Formula.t
      }
    [@@deriving sexp]

    type t =
      { id : Id.t
      ; values : values
      ; xy : formula list
      }
    [@@deriving sexp_of]

    let calc { id; values; xy } ~scoped_values ~t =
      let c = Expr.calc ~values:(Map.find_exn values) ~scoped_values (module N) in
      let calc_xy f =
        Formula.to_polynomial f ~values:(Map.find_exn values) ~scoped_values
        |> Solver.Polynomial.calc ~x:t
      in
      List.find_map xy ~f:(fun { interval; x; y } ->
          match interval with
          | `Interval (l, r) ->
            if N.(c l <= t && t < c r) then Some (calc_xy x, calc_xy y) else None
          | `PosInfinity l -> if N.(c l <= t) then Some (calc_xy x, calc_xy y) else None)
    ;;

    let update body key value =
      { body with
        values =
          Map.update body.values key ~f:(function
              | Some _ -> value
              | None -> assert false)
      }
    ;;

    let update_x0y0 body (x, y) =
      let values = body.values in
      let values =
        Map.update values `x0 ~f:(function
            | Some _ -> Scalar x
            | None -> assert false)
      in
      let values =
        Map.update values `y0 ~f:(function
            | Some _ -> Scalar y
            | None -> assert false)
      in
      { body with values }
    ;;

    module Sample = struct
      module Formulas = struct
        let b2 a b ~r =
          let p =
            Sequence.cartesian_product (Sequence.of_list a.xy) (Sequence.of_list b.xy)
          in
          let p1 =
            Sequence.map p ~f:(fun (a, b) ->
                let open Formula.Syntax in
                let x_1 = scope a.x ~scope:1 in
                let x_2 = scope b.x ~scope:2 in
                let y_1 = scope a.y ~scope:1 in
                let y_2 = scope b.y ~scope:2 in
                let r_1 = scope r ~scope:1 in
                let r_2 = scope r ~scope:2 in
                let f = sqr (x_2 - x_1) + sqr (y_2 - y_1) - sqr (r_1 + r_2) in
                a.interval, b.interval, f)
          in
          p1
        ;;

        let b3 p a b ~global =
          let inter body =
            let calc v =
              Expr.calc
                ~values:(Map.find_exn body.values)
                ~scoped_values:(function
                  | -1 -> global
                  | _ -> assert false)
                (module N)
                v
            in
            function
            | `Interval (l, r) -> `Interval (calc l, calc r)
            | `PosInfinity l -> `PosInfinity (calc l)
          in
          let p1 =
            Sequence.map p ~f:(fun (ai, bi, f) ->
                ( inter a ai
                , inter b bi
                , Formula.to_polynomial f ~values:global ~scoped_values:(function
                      | -1 -> global
                      | 1 -> Map.find_exn a.values
                      | 2 -> Map.find_exn b.values
                      | _ -> assert false) ))
          in
          p1
        ;;

        let b4 p ~eps =
          let is_in_interval n = function
            | `Interval (l, r) -> N.(l <= n && n < r)
            | `PosInfinity l -> N.(l <= n)
          in
          let p1 =
            Sequence.filter_map p ~f:(fun (ai, bi, p) ->
                let roots =
                  try Solver.PolynomialEquation.roots p ~eps with
                  | _ -> []
                in
                match
                  roots
                  |> List.filter ~f:(fun root ->
                         is_in_interval root ai && is_in_interval root bi)
                with
                | [] -> None
                | l -> Some (Sequence.of_list l))
          in
          p1 |> Sequence.concat
        ;;
      end
    end
  end

  module Scene = struct
    type figures = (Figure2.Id.t, Figure2.t, Figure2.Id.comparator_witness) Map.t

    let sexp_of_figures a = [%sexp (Map.to_alist a : (Figure2.Id.t * Figure2.t) list)]

    type t =
      { figures : figures
      ; global_values : values
      }
    [@@deriving sexp_of]

    let figures { figures; _ } = figures

    module Exprs = struct
      open Expr.Syntax

      let g = scope ~scope:(-1) (fst @@ scalar_var `g)
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
    end

    module Formulas = struct
      open Exprs
      open Expr.Syntax

      let x = Formula.of_alist_exn [ 0, x0; 1, v0_x; 2, a_x * half ]
      let y = Formula.of_alist_exn [ 0, y0; 1, v0_y; 2, a_y * half ]
      let x_after = Formula.of_alist_exn [ 0, x0 + (three * sqr v0_x / (two * a_x)) ]
      let y_after = Formula.of_alist_exn [ 0, y0 + (three * sqr v0_y / (two * a_y)) ]
      let r = Formula.of_alist_exn [ 0, r ]
    end

    let add_figure figures ~id ~x0 ~y0 ~r ~mu =
      Map.add_exn
        figures
        ~key:id
        ~data:
          Figure2.
            { id
            ; values =
                Map.of_alist_exn
                  (module Vars)
                  [ `x0, Expr.Scalar x0
                  ; `y0, Expr.Scalar y0
                  ; `v0, Expr.Vector (N.zero, N.zero)
                  ; `r, Expr.Scalar r
                  ; `mu, Expr.Scalar mu
                  ]
            ; xy =
                Figure2.
                  [ { interval = `Interval (Exprs.border_l1, Exprs.border_r1)
                    ; x = Formulas.x
                    ; y = Formulas.y
                    }
                  ; { interval = `PosInfinity Exprs.border_l2
                    ; x = Formulas.x_after
                    ; y = Formulas.y_after
                    }
                  ]
            }
    ;;

    let scene ~g =
      { figures = Map.empty (module Figure2.Id)
      ; global_values = Map.of_alist_exn (module Vars) [ `g, Expr.Scalar g ]
      }
    ;;

    let update_coords ({ figures; global_values } as scene) ~t =
      let q =
        Map.map figures ~f:(fun f ->
            let xy =
              Figure2.calc
                f
                ~scoped_values:(function
                  | -1 -> Map.find_exn global_values
                  | _ -> assert false)
                ~t
            in
            match xy with
            | Some xy -> Figure2.update_x0y0 f xy
            | None -> f)
      in
      { scene with figures = q }
    ;;

    let t ~eps scene =
      let scene = scene in
      let seq = Map.to_sequence scene.figures in
      let p = Sequence.cartesian_product seq seq in
      let q =
        Sequence.map p ~f:(fun ((id1, f1), (id2, f2)) ->
            let q2 = Figure2.Sample.Formulas.b2 f1 f2 ~r:Formulas.r in
            let q3 =
              Figure2.Sample.Formulas.b3
                q2
                f1
                f2
                ~global:(Map.find_exn scene.global_values)
            in
            let q4 = Figure2.Sample.Formulas.b4 q3 ~eps in
            id1, id2, q4)
      in
      q
    ;;

    (* let t1 scene = let scene = scene in let seq = Map.to_sequence scene.figures in let
       p = Sequence.cartesian_product seq seq in let q = Sequence.map p ~f:(fun ((_id1,
       f1), (_id2, f2)) -> Figure2.Sample.Formulas.b1 f1 f2 ~r:Formulas.r) in q ;; *)

    (* let a = t (scene ()) *)
  end

  module Action = struct
    type a =
      | AddBody of
          { id : Figure2.Id.t
          ; x0 : N.t
          ; y0 : N.t
          ; r : N.t
          ; mu : N.t
          }
      | GiveVelocity of
          { id : Figure2.Id.t
          ; v0 : N.t * N.t
          }
      | Empty
    [@@deriving sexp]

    type t =
      { time : N.t
      ; action : a
      }
    [@@deriving sexp]
  end

  module Events = struct
    type t = SuccessfulAction of Action.t [@@deriving sexp]
  end

  module Model = struct
    type t = (N.t, Scene.t, N.comparator_witness) Map.t

    let empty ~g : t = Map.of_alist_exn (module N) [ N.zero, Scene.scene ~g ]
  end

  module Engine = struct
    let recv model (Action.{ time; action } as a) =
      match Map.max_elt model with
      | Some (old_time, _) when N.(old_time > time) ->
        Error.raise_s
          [%message "Unimplemented" ~time:(time : N.t) ~old_time:(old_time : N.t)]
      | Some (old_time, s) ->
        let s = Scene.update_coords s ~t:N.(time - old_time) in
        begin
          match action with
          | Action.AddBody { id; x0; y0; r; mu } ->
            ( Map.add_exn
                model
                ~key:time
                ~data:Scene.{ s with figures = add_figure s.figures ~id ~x0 ~y0 ~r ~mu }
            , [ Events.SuccessfulAction a ] )
          | GiveVelocity { id; v0 } ->
            let body = Map.find_exn s.figures id in
            let body =
              { body with
                values =
                  Map.update body.values `v0 ~f:(function
                      | Some _ -> Vector v0
                      | None -> assert false)
              }
            in
            ( Map.update model time ~f:(fun _ ->
                  Scene.
                    { s with
                      figures =
                        Map.update s.figures id ~f:(function
                            | Some _ -> body
                            | None -> assert false)
                    })
            , [ Events.SuccessfulAction a ] )
          | Empty -> Map.update model time ~f:(fun _ -> s), [ Events.SuccessfulAction a ]
        end
      | None -> assert false, []
    ;;
  end
end
