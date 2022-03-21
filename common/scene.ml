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

    module Id = struct
      type t
    end
  end

  module Figure2 = struct
    module Expr = Expr.Make (String) (Int) (N)
    module Formula = Formula.Make (String) (Int) (N) (Expr) (Solver)

    type formula =
      { interval : Expr.scalar Expr.t * Expr.scalar Expr.t
      ; x : Formula.t
      ; y : Formula.t
      }

    type values = (string, Expr.value, String.comparator_witness) Map.t

    let sexp_of_values values = [%sexp (Map.to_alist values : (string * Expr.value) list)]

    type t =
      { id : int
      ; values : values
      ; x : Formula.t
      ; y : Formula.t (* ; xy : 'a formula list *)
      }
    [@@deriving sexp_of]

    module Sample = struct
      module Formulas = struct
        let b a b v ~r =
          let open Formula.Syntax in
          let x_1 = scope a.x ~scope:1 in
          let x_2 = scope b.x ~scope:2 in
          let y_1 = scope a.y ~scope:1 in
          let y_2 = scope b.y ~scope:2 in
          let r_1 = scope r ~scope:1 in
          let r_2 = scope r ~scope:2 in
          sqr (x_2 - x_1) + sqr (y_2 - y_1) - sqr (r_1 + r_2)
          |> Formula.to_polynomial ~values:v ~scoped_values:(function
                 | 1 -> Map.find_exn a.values
                 | 2 -> Map.find_exn b.values
                 | _ -> v)
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
    type t =
      { figures : (int, Figure2.t, Int.comparator_witness) Map.t
      ; global_values : Figure2.Expr.values
      }

    let figures { figures; _ } = figures

    module Vars1 = struct
      open Figure2.Expr.Syntax

      let a_vec, a_vec_name = vector_var "a_vec"
      let a_x = vector_x a_vec
      let a_y = vector_y a_vec
      let v0_vec, v0_vec_name = vector_var "v0_vec"
      let v0_x = vector_x v0_vec
      let v0_y = vector_y v0_vec
      let x0, x0_name = scalar_var "x0"
      let y0, y0_name = scalar_var "y0"
      let r, r_name = scalar_var "r"
      let half = scalar_const N.(one / (one + one))
    end

    module Formulas1 = struct
      open Vars1
      open Figure2.Expr.Syntax

      let x = Map.of_alist_exn (module Int) [ 0, x0; 1, v0_x; 2, a_x * half ]
      let y = Map.of_alist_exn (module Int) [ 0, y0; 1, v0_y; 2, a_y * half ]
    end

    let scene () =
      let open Figure2.Expr.Syntax in
      let a_vec, a_vec_name = vector_var "a_vec" in
      let a_x = vector_x a_vec in
      let a_y = vector_y a_vec in
      let v0_vec, v0_vec_name = vector_var "v0_vec" in
      let v0_x = vector_x v0_vec in
      let v0_y = vector_y v0_vec in
      let x0, x0_name = scalar_var "x0" in
      let y0, y0_name = scalar_var "y0" in
      let _r, r_name = scalar_var "r" in
      let half = scalar_const N.(one / (one + one)) in
      let x = Figure2.Formula.of_alist_exn [ 0, x0; 1, v0_x; 2, a_x * half ] in
      let y = Figure2.Formula.of_alist_exn [ 0, y0; 1, v0_y; 2, a_y * half ] in
      { figures =
          Map.of_alist_exn
            (module Int)
            [ ( 0
              , { Figure2.id = 0
                ; values =
                    Map.of_alist_exn
                      (module String)
                      [ a_vec_name, Figure2.Expr.Vector N.(one, -one / (one + one))
                      ; ( v0_vec_name
                        , Figure2.Expr.Vector N.(-(one + one), one / (one + one)) )
                      ; x0_name, Figure2.Expr.Scalar N.(-(one + one + one))
                      ; y0_name, Figure2.Expr.Scalar N.(one + one + one)
                      ; r_name, Figure2.Expr.Scalar N.(one + one + one + one)
                      ]
                ; x
                ; y (* ; xy = [] *)
                } )
            ; ( 1
              , { Figure2.id = 1
                ; values =
                    Map.of_alist_exn
                      (module String)
                      [ a_vec_name, Figure2.Expr.Vector N.(one, one / (one + one))
                      ; v0_vec_name, Figure2.Expr.Vector N.(one + one, one / (one + one))
                      ; x0_name, Figure2.Expr.Scalar N.(one + one + one)
                      ; y0_name, Figure2.Expr.Scalar N.(one + one + one)
                      ; r_name, Figure2.Expr.Scalar N.(one)
                      ]
                ; x
                ; y (* ; xy = [] *)
                } )
            ]
      ; global_values = (fun _ -> assert false)
      }
    ;;

    let t scene =
      let scene = scene in
      let seq = Map.to_sequence scene.figures in
      let p = Sequence.cartesian_product seq seq in
      let r, _ = Figure2.Expr.Syntax.scalar_var "r" in
      let r = Figure2.Formula.of_alist_exn [ 0, r ] in
      let q =
        Sequence.map p ~f:(fun ((_id1, f1), (_id2, f2)) ->
            Figure2.Sample.Formulas.b f1 f2 (fun _ -> assert false) ~r)
      in
      q
    ;;

    let t1 scene =
      let scene = scene in
      let seq = Map.to_sequence scene.figures in
      let p = Sequence.cartesian_product seq seq in
      let r, _ = Figure2.Expr.Syntax.scalar_var "r" in
      let r = Figure2.Formula.of_alist_exn [ 0, r ] in
      let q =
        Sequence.map p ~f:(fun ((_id1, f1), (_id2, f2)) ->
            Figure2.Sample.Formulas.b1 f1 f2 ~r)
      in
      q
    ;;

    let a = t (scene ())
  end

  module Events = struct
    type t =
      | Init of (int, Figure2.t, Int.comparator_witness) Map.t
      | BodiesMoved
  end

  module Action = struct
    type t =
      | GiveVelocity of Time_ns.Span.t * Figure.Id.t * Figure.Velocity.t Figure.Vec.t
  end

  module Model = struct
    type t = (Time_ns.Span.t, Scene.t, Time_ns.Span.comparator_witness) Map.t

    let e () : t =
      Map.of_alist_exn (module Time_ns.Span) [ Time_ns.Span.zero, Scene.scene () ]
    ;;
  end

  module Engine = struct
    let recv model _action = model, assert false
  end
end
