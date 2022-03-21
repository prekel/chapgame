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

    (* type formula = { interval : [ `Interval of N.t Expr.t * N.t Expr.t | `PosInfinity
       of N.t Expr.t ] ; x : Formula.t ; y : Formula.t } *)

    type t =
      { id : Id.t
      ; values : values
      ; x : Formula.t
      ; y : Formula.t (* ; xy : 'a formula list *)
      }
    [@@deriving sexp_of]

    module Sample = struct
      module Formulas = struct
        let b a b ~global ~r =
          let open Formula.Syntax in
          let x_1 = scope a.x ~scope:1 in
          let x_2 = scope b.x ~scope:2 in
          let y_1 = scope a.y ~scope:1 in
          let y_2 = scope b.y ~scope:2 in
          let r_1 = scope r ~scope:1 in
          let r_2 = scope r ~scope:2 in
          sqr (x_2 - x_1) + sqr (y_2 - y_1) - sqr (r_1 + r_2)
          |> Formula.to_polynomial ~values:global ~scoped_values:(function
                 | -1 -> global
                 | 1 -> Map.find_exn a.values
                 | 2 -> Map.find_exn b.values
                 | _ -> assert false)
        ;;

        let b1 a b ~r =
          let open Formula.Syntax in
          let x_1 = scope a.x ~scope:1 in
          let x_2 = scope b.x ~scope:2 in
          let y_1 = scope a.y ~scope:1 in
          let y_2 = scope b.y ~scope:2 in
          let r_1 = scope r ~scope:1 in
          let r_2 = scope r ~scope:2 in
          sqr (x_2 - x_1) + sqr (y_2 - y_1) - sqr (r_1 + r_2)
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
    end

    module Formulas = struct
      open Exprs
      open Expr.Syntax

      let x = Formula.of_alist_exn [ 0, x0; 1, v0_x; 2, a_x * half ]
      let y = Formula.of_alist_exn [ 0, y0; 1, v0_y; 2, a_y * half ]
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
            ; x = Formulas.x
            ; y = Formulas.y
            }
    ;;

    let scene ~g =
      { figures = Map.empty (module Figure2.Id)
      ; global_values = Map.of_alist_exn (module Vars) [ `g, Expr.Scalar g ]
      }
    ;;

    let t scene =
      let scene = scene in
      let seq = Map.to_sequence scene.figures in
      let p = Sequence.cartesian_product seq seq in
      let q =
        Sequence.map p ~f:(fun ((_id1, f1), (_id2, f2)) ->
            Figure2.Sample.Formulas.b
              f1
              f2
              ~global:(Map.find_exn scene.global_values)
              ~r:Formulas.r)
      in
      q
    ;;

    let t1 scene =
      let scene = scene in
      let seq = Map.to_sequence scene.figures in
      let p = Sequence.cartesian_product seq seq in
      let q =
        Sequence.map p ~f:(fun ((_id1, f1), (_id2, f2)) ->
            Figure2.Sample.Formulas.b1 f1 f2 ~r:Formulas.r)
      in
      q
    ;;

    (* let a = t (scene ()) *)
  end

  module Action = struct
    type a =
      | AddBody of
          { x0 : N.t
          ; y0 : N.t
          ; r : N.t
          ; mu : N.t
          }
    (* | GiveVelocity of Time_ns.Span.t * Figure2.Id.t * Figure.Velocity.t Figure.Vec.t *)
    [@@deriving sexp]

    type t =
      { time : Time_ns.Span.t
      ; action : a
      }
    [@@deriving sexp]
  end

  module Events = struct
    type t = SuccessfulAction of Action.t [@@deriving sexp]
  end

  module Model = struct
    type t = (Time_ns.Span.t, Scene.t, Time_ns.Span.comparator_witness) Map.t

    let empty ~g : t =
      Map.of_alist_exn (module Time_ns.Span) [ Time_ns.Span.zero, Scene.scene ~g ]
    ;;
  end

  module Engine = struct
    let recv model (Action.{ time; action } as a) =
      match action with
      | Action.AddBody { x0; y0; r; mu } ->
        begin
          match Map.max_elt model with
          | Some (max_time, _) when Time_ns.Span.(max_time > time) ->
            Error.raise_s
              [%message
                "Unimplemented"
                  ~time:(time : Time_ns.Span.t)
                  ~max_time:(max_time : Time_ns.Span.t)]
          | Some (_, s) ->
            ( Map.add_exn
                model
                ~key:time
                ~data:
                  Scene.
                    { s with
                      figures =
                        add_figure s.figures ~id:(Figure2.Id.next ()) ~x0 ~y0 ~r ~mu
                    }
            , [ Events.SuccessfulAction a ] )
          | None -> assert false, []
        end
    ;;
  end
end
