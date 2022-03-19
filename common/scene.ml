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

    type t =
      { id : Id.t
      ; m : Kg.t
      ; v0 : Velocity.t option
      ; f : Force.t
      ; r : Metre.t
      ; x : Metre.t
      ; y : Metre.t
      }

    let collision : t -> t -> float = assert false
  end

  module Figure2 = struct
    module Formula = struct
      module VarId = struct
        type t =
          | Global of string
          | Body of string
      end

      module Var = struct
        type scalar_key = string
        type vector_key = string
        type scalar = N.t
        type vector = N.t * N.t
        type scalar_values = (scalar_key, scalar, String.comparator_witness) Map.t
        type vector_values = (vector_key, vector, String.comparator_witness) Map.t

        type ('scope, 'inner) t =
          | ScalarConst : scalar -> ('static, scalar) t
          | VectorConst : vector -> ('static, vector) t
          | ScalarGlobalVar : scalar_key -> ([> `Global ], scalar) t
          | VectorGlobalVar : vector_key -> ([> `Global ], vector) t
          | ScalarVar : scalar_key -> ('scope, scalar) t
          | VectorVar : vector_key -> ('scope, vector) t
          | Sum : ('scope, 'a) t * ('scope, 'a) t -> ('scope, 'a) t
          | Sub : ('scope, 'a) t * ('scope, 'a) t -> ('scope, 'a) t
          | Mult : ('scope, 'a) t * ('scope, 'a) t -> ('scope, 'a) t
          | Inv : ('scope, 'a) t -> ('scope, 'a) t
          | XOfVector : ('scope, vector) t -> ('scope, scalar) t
          | YOfVector : ('scope, vector) t -> ('scope, scalar) t
          | LengthOfVector : ('scope, vector) t -> ('scope, scalar) t
          | Scope : 'scope * ('scope, 'a) t -> ('scope, 'a) t

        module type ScalarVec = sig
          type t

          val ( + ) : t -> t -> t
          val ( * ) : t -> t -> t
          val ( ~- ) : t -> t
        end

        module FV = struct
          open N

          type t = N.t * N.t

          let ( + ) (x1, y1) (x2, y2) = x1 + x2, y1 + y2
          let ( * ) (x1, y1) (x2, y2) = x1 * x2, y1 * y2
          let ( ~- ) (x, y) = -x, -y
        end

        let rec calc
            : type scope result.
              scalar_values
              -> vector_values
              -> (scope -> scalar_values * vector_values)
              -> (module ScalarVec with type t = result)
              -> (scope, result) t
              -> result
          =
         fun scalar_values vector_values scope_values (module ScalarVec) ->
          let open ScalarVec in
          function
          | ScalarConst x -> x
          | VectorConst x -> x
          | ScalarVar name -> Map.find_exn scalar_values name
          | VectorVar name -> Map.find_exn vector_values name
          | ScalarGlobalVar name ->
            let s, _ = scope_values `Global in
            Map.find_exn s name
          | VectorGlobalVar name ->
            let _, v = scope_values `Global in
            Map.find_exn v name
          | Sum (a, b) ->
            let calc = calc scalar_values vector_values scope_values (module ScalarVec) in
            let ca = calc a in
            let cb = calc b in
            ca + cb
          | Sub (a, b) ->
            let calc = calc scalar_values vector_values scope_values (module ScalarVec) in
            let ca = calc a in
            let cb = calc b in
            ca + -cb
          | Mult (a, b) ->
            let calc = calc scalar_values vector_values scope_values (module ScalarVec) in
            let ca = calc a in
            let cb = calc b in
            ca * cb
          | Inv a ->
            let calc = calc scalar_values vector_values scope_values (module ScalarVec) in
            let ca = calc a in
            -ca
          | XOfVector v ->
            let calc = calc scalar_values vector_values scope_values (module FV) in
            let x, _y = calc v in
            x
          | YOfVector v ->
            let calc = calc scalar_values vector_values scope_values (module FV) in
            let _x, y = calc v in
            y
          | LengthOfVector v ->
            let calc = calc scalar_values vector_values scope_values (module FV) in
            let x, y = calc v in
            N.(sqrt ((x * x) + (y * y)))
          | Scope (scope, x) ->
            let s, v = scope_values scope in
            calc s v scope_values (module ScalarVec) x
       ;;
      end

      type 'a t = (int, ('a, Var.scalar) Var.t, Int.comparator_witness) Map.t

      let ( + ) = Map.merge_skewed ~combine:(fun ~key:_ a b -> Var.Sum (a, b))
      let ( ~- ) = Map.map ~f:(fun v -> Var.Inv v)

      let ( * ) a b =
        Map.fold
          a
          ~init:(Map.empty (module Int))
          ~f:(fun ~key:degree1 ~data:coef1 acc ->
            Map.merge_skewed
              acc
              (Map.map b ~f:(fun coef2 -> Var.Mult (coef1, coef2))
              |> Map.map_keys_exn (module Int) ~f:(fun degree2 -> degree1 * degree2))
              ~combine:(fun ~key:_ v1 v2 -> Var.Sum (v1, v2)))
      ;;

      (* let (_ : t) = Map.of_alist_exn (module Int) [ 1, Var.ScalarVar "" ]*)

      let to_polynomial p ~s ~v ~sc : Solver.Polynomial.t =
        Map.map p ~f:(fun a -> Var.calc s v sc (module N) a) |> Solver.Polynomial.of_map
      ;;
    end

    type 'a t =
      { id : int
      ; scalar_values : Formula.Var.scalar_values
      ; vector_values : Formula.Var.vector_values
      ; x : 'a Formula.t
      ; y : 'a Formula.t
      }

    module FormulaScored = struct
      type 'a t =
        { scope : 'a
        ; formula : 'a Formula.t
        }

      let create ~scope formula = { scope; formula }
      let formula { scope = _; formula } = formula

      let ( + )
          wanted_scope
          { scope = scope1; formula = formula1 }
          { scope = scope2; formula = formula2 }
        =
        { scope = wanted_scope (scope1, scope2)
        ; formula =
            Map.merge_skewed formula1 formula2 ~combine:(fun ~key:_ a b ->
                Formula.Var.Sum
                  (Formula.Var.Scope (scope1, a), Formula.Var.Scope (scope2, b)))
        }
      ;;

      let ( - )
          wanted_scope
          { scope = scope1; formula = formula1 }
          { scope = scope2; formula = formula2 }
        =
        { scope = wanted_scope (scope1, scope2)
        ; formula =
            Map.merge_skewed formula1 formula2 ~combine:(fun ~key:_ a b ->
                Formula.Var.Sub
                  (Formula.Var.Scope (scope1, a), Formula.Var.Scope (scope2, b)))
        }
      ;;

      let ( ~- ) { scope; formula } =
        Map.map formula ~f:(fun v -> Formula.Var.Inv (Formula.Var.Scope (scope, v)))
      ;;

      let ( * )
          wanted_scope
          { scope = scope1; formula = formula1 }
          { scope = scope2; formula = formula2 }
        =
        { scope = wanted_scope (scope1, scope2)
        ; formula =
            Map.fold
              formula1
              ~init:(Map.empty (module Int))
              ~f:(fun ~key:degree1 ~data:coef1 acc ->
                Map.merge_skewed
                  acc
                  (Map.map formula2 ~f:(fun coef2 ->
                       Formula.Var.Mult
                         ( Formula.Var.Scope (scope1, coef1)
                         , Formula.Var.Scope (scope2, coef2) ))
                  |> Map.map_keys_exn (module Int) ~f:(fun degree2 -> degree1 * degree2))
                  ~combine:(fun ~key:_ v1 v2 -> Formula.Var.Sum (v1, v2)))
        }
      ;;
    end

    module Sample : sig
      module Formulas : sig
        val f
          :  Formula.Var.scalar_values
          -> Formula.Var.vector_values
          -> Formula.Var.scalar_values
          -> Formula.Var.vector_values
          -> Formula.Var.scalar_values
          -> Formula.Var.vector_values
          -> Solver.Polynomial.t
      end
    end = struct
      module Vars = struct
        open Formula.Var

        let a_vec = VectorVar "a_vec"
        let a_x = XOfVector a_vec
        let a_y = YOfVector a_vec
        let v0_vec = VectorVar "v0_vec"
        let v0_x = XOfVector v0_vec
        let v0_y = YOfVector v0_vec
        let x0 = ScalarVar "x0"
        let y0 = ScalarVar "y0"
        let r = ScalarVar "r"
        let half = ScalarConst N.(one / (one + one))
        (* let g = ScalarGlobalVar "g" *)
      end

      module Formulas = struct
        open Vars

        let x = Map.of_alist_exn (module Int) [ 0, x0; 1, v0_x; 2, Mult (a_x, half) ]
        let y = Map.of_alist_exn (module Int) [ 0, y0; 1, v0_y; 2, Mult (a_y, half) ]

        (* let g = Map.of_alist_exn (module Int) [ 0, g ] *)
        let r = Map.of_alist_exn (module Int) [ 0, r ]

        let f s v s1 v1 s2 v2 =
          let open FormulaScored in
          let x_1 = create x ~scope:`Figure1 in
          let x_2 = create x ~scope:`Figure2 in
          let y_1 = create y ~scope:`Figure1 in
          let y_2 = create y ~scope:`Figure2 in
          let r_1 = create r ~scope:`Figure1 in
          let r_2 = create r ~scope:`Figure2 in
          let ( + ) = ( + ) (fun (a, b) -> `Scope (a, b)) in
          let ( - ) = ( - ) (fun (a, b) -> `Scope (a, b)) in
          let ( * ) = ( * ) (fun (a, b) -> `Scope (a, b)) in
          ((x_2 - x_1) * (x_2 - x_1))
          + ((y_2 - y_1) * (y_2 - y_1))
          - ((r_1 + r_2) * (r_1 + r_2))
          |> formula
          |> Formula.to_polynomial ~s ~v ~sc:(function
                 | `Figure1 -> s1, v1
                 | `Figure2 -> s2, v2
                 | `Global -> s, v
                 | `Scope (_a, _b) -> s, v)
        ;;
      end
    end
  end

  module Scene = struct
    type t = { figures : (Figure.Id.t, Figure.t, unit) Map.t }
  end

  module Events = struct
    type t =
      | Init of (Figure.Id.t, Figure.t, unit) Map.t
      | BodiesMoved
  end

  module Action = struct
    type t =
      | GiveVelocity of Time_ns.Span.t * Figure.Id.t * Figure.Velocity.t Figure.Vec.t
  end

  module Model = struct
    type t = (Time_ns.Span.t, Scene.t, Time_ns.Span.comparator_witness) Map.t

    let e () : t = Map.empty (module Time_ns.Span)
  end

  module Engine = struct
    let recv model _action = model, assert false
  end
end