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
        type key = string
        type scalar = N.t [@@deriving sexp]
        type vector = N.t * N.t [@@deriving sexp]

        type value =
          | Scalar of scalar
          | Vector of vector

        let scalar_exn = function
          | Scalar s -> s
          | Vector v ->
            Error.raise_s [%message "Expected scalar, got vector" ~v:(v : vector)]
        ;;

        let vector_exn = function
          | Scalar s ->
            Error.raise_s [%message "Expected vector, got scalar" ~s:(s : scalar)]
          | Vector v -> v
        ;;

        type values = (key, value, String.comparator_witness) Map.t

        type ('scope, 'inner) t =
          | ScalarConst : scalar -> ('static, scalar) t
          | VectorConst : vector -> ('static, vector) t
          | ScalarGlobalVar : key -> ([> `Global ], scalar) t
          | VectorGlobalVar : key -> ([> `Global ], vector) t
          | ScalarVar : key -> ('scope, scalar) t
          | VectorVar : key -> ('scope, vector) t
          | Sum : ('scope, 'a) t * ('scope, 'a) t -> ('scope, 'a) t
          | Sub : ('scope, 'a) t * ('scope, 'a) t -> ('scope, 'a) t
          | Sqr : ('scope, 'a) t -> ('scope, 'a) t
          | Mult : ('scope, 'a) t * ('scope, 'a) t -> ('scope, 'a) t
          | Div : ('scope, 'a) t * ('scope, 'a) t -> ('scope, 'a) t
          | Neg : ('scope, 'a) t -> ('scope, 'a) t
          | XOfVector : ('scope, vector) t -> ('scope, scalar) t
          | YOfVector : ('scope, vector) t -> ('scope, scalar) t
          | LengthOfVector : ('scope, vector) t -> ('scope, scalar) t
          | Scope : 'scope * ('old_scope, 'a) t -> ('scope, 'a) t

        module type ScalarVec = sig
          type t

          val ( + ) : t -> t -> t
          val ( * ) : t -> t -> t
          val ( / ) : t -> t -> t
          val ( ~- ) : t -> t
        end

        module FV = struct
          open N

          type t = N.t * N.t

          let ( + ) (x1, y1) (x2, y2) = x1 + x2, y1 + y2
          let ( * ) (x1, y1) (x2, y2) = x1 * x2, y1 * y2
          let ( / ) (x1, y1) (x2, y2) = x1 / x2, y1 / y2
          let ( ~- ) (x, y) = -x, -y
        end

        let rec calc
            : type old_scope scope result.
              values:values
              -> scoped_values:(scope -> values)
              -> (module ScalarVec with type t = result)
              -> (scope, result) t
              -> result
          =
         fun ~values ~scoped_values (module ScalarVec) -> function
          | ScalarConst x -> x
          | VectorConst x -> x
          | ScalarVar name -> Map.find_exn values name |> scalar_exn
          | VectorVar name -> Map.find_exn values name |> vector_exn
          | ScalarGlobalVar name ->
            Map.find_exn (scoped_values `Global) name |> scalar_exn
          | VectorGlobalVar name ->
            Map.find_exn (scoped_values `Global) name |> vector_exn
          | Sum (a, b) ->
            let calc = calc ~values ~scoped_values (module ScalarVec) in
            let ca = calc a in
            let cb = calc b in
            ScalarVec.(ca + cb)
          | Sub (a, b) ->
            let calc = calc ~values ~scoped_values (module ScalarVec) in
            let ca = calc a in
            let cb = calc b in
            ScalarVec.(ca + -cb)
          | Sqr a ->
            let ca = calc ~values ~scoped_values (module ScalarVec) a in
            ScalarVec.(ca * ca)
          | Mult (a, b) ->
            let calc = calc ~values ~scoped_values (module ScalarVec) in
            let ca = calc a in
            let cb = calc b in
            ScalarVec.(ca * cb)
          | Div (a, b) ->
            let calc = calc ~values ~scoped_values (module ScalarVec) in
            let ca = calc a in
            let cb = calc b in
            ScalarVec.(ca / cb)
          | Neg a ->
            let ca = calc ~values ~scoped_values (module ScalarVec) a in
            ScalarVec.(-ca)
          | XOfVector v ->
            let x, _y = calc ~values ~scoped_values (module FV) v in
            x
          | YOfVector v ->
            let _x, y = calc ~values ~scoped_values (module FV) v in
            y
          | LengthOfVector v ->
            let x, y = calc ~values ~scoped_values (module FV) v in
            N.(sqrt ((x * x) + (y * y)))
          | Scope (scope, x) ->
            calc ~values:(scoped_values scope) ~scoped_values (module ScalarVec) x
       ;;

        module Infix = struct
          let scalar_var s = ScalarVar s
          let vector_var v = VectorVar v
          let scalar_const s = ScalarConst s
          let vector_const v = VectorConst v
          let scalar_global s = ScalarGlobalVar s
          let vector_global v = VectorGlobalVar v
          let ( + ) a b = Sum (a, b)
          let ( - ) a b = Sub (a, b)
          let ( * ) a b = Mult (a, b)
          let ( / ) a b = Div (a, b)
          let ( ~- ) a = Neg a
          let vector_length v = LengthOfVector v
          let vector_x v = XOfVector v
          let vector_y v = YOfVector v
          let scope ~scope a = Scope (scope, a)
        end
      end

      type 'a t = (int, ('a, Var.scalar) Var.t, Int.comparator_witness) Map.t

      let ( + ) = Map.merge_skewed ~combine:(fun ~key:_ a b -> Var.Sum (a, b))
      let ( ~- ) = Map.map ~f:(fun v -> Var.Neg v)

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

      let to_polynomial p ~values ~scoped_values : Solver.Polynomial.t =
        Map.map p ~f:(fun a -> Var.calc ~values ~scoped_values (module N) a)
        |> Solver.Polynomial.of_map
      ;;
    end

    type 'a t =
      { id : int
      ; values : Formula.Var.values
      ; x : 'a Formula.t
      ; y : 'a Formula.t
      }

    module FormulaScored = struct
      let ( + ) = Map.merge_skewed ~combine:(fun ~key:_ a b -> Formula.Var.Sum (a, b))
      let ( - ) = Map.merge_skewed ~combine:(fun ~key:_ a b -> Formula.Var.Sub (a, b))

      (* let ( ~- ) { scope; formula } = Map.map formula ~f:(fun v -> Formula.Var.Neg
         (Formula.Var.Scope (scope, v))) ;;

         let ( * ) wanted_scope { scope = scope1; formula = formula1 } { scope = scope2;
         formula = formula2 } = { scope = wanted_scope ; formula = Map.fold formula1
         ~init:(Map.empty (module Int)) ~f:(fun ~key:degree1 ~data:coef1 acc ->
         Map.merge_skewed acc (Map.map formula2 ~f:(fun coef2 -> Formula.Var.Mult (
         Formula.Var.Scope (scope1, coef1) , Formula.Var.Scope (scope2, coef2) )) |>
         Map.map_keys_exn (module Int) ~f:(fun degree2 -> degree1 * degree2))
         ~combine:(fun ~key:_ v1 v2 -> Formula.Var.Sum (v1, v2))) } ;; *)
      let sqr = Map.map ~f:(fun v -> Formula.Var.Sqr v)
    end

    module Sample : sig
      type 'a scope =
        [< `Body1 | `Body2 | `Global | `Phantom > `Body1 `Body2 `Phantom ] as 'a

      module Formulas : sig end
    end = struct
      type 'a scope =
        [< `Body1 | `Body2 | `Global | `Phantom > `Body1 `Body2 `Phantom ] as 'a

      module Vars = struct
        open Formula.Var.Infix

        let a_vec = vector_var "a_vec"
        let a_x = vector_x a_vec
        let a_y = vector_y a_vec
        let v0_vec = vector_var "v0_vec"
        let v0_x = vector_x v0_vec
        let v0_y = vector_y v0_vec
        let x0 = scalar_var "x0"
        let y0 = scalar_var "y0"
        let r = scalar_var "r"
        let half = scalar_const N.(one / (one + one))
        let g = scalar_global "g"
      end

      module Formulas = struct
        open Vars
        open Formula.Var.Infix

        let x = Map.of_alist_exn (module Int) [ 0, x0; 1, v0_x; 2, a_x * half ]
        let y = Map.of_alist_exn (module Int) [ 0, y0; 1, v0_y; 2, a_y * half ]
        let _g = Map.of_alist_exn (module Int) [ 0, g ]
        let r = Map.of_alist_exn (module Int) [ 0, r ]

        (* let f v v1 v2 = let open FormulaScored in let x_1 = create x ~scope:`Figure1 in
           let x_2 = create x ~scope:`Figure2 in let y_1 = create y ~scope:`Figure1 in let
           y_2 = create y ~scope:`Figure2 in let r_1 = create r ~scope:`Figure1 in let r_2
           = create r ~scope:`Figure2 in let ( + ) = ( + ) `Scope in let ( - ) = ( - )
           `Scope in sqr (x_2 - x_1) + sqr (y_2 - y_1) - sqr (r_1 + r_2) |> formula |>
           Formula.to_polynomial ~values:v ~scoped_values:(function | `Figure1 -> v1 |
           `Figure2 -> v2 | `Global -> v | `Scope -> v | `Local -> v | `Static -> v) ;; *)

        let scope m ~scope = Map.map m ~f:(fun v -> Formula.Var.Scope (scope, v))

        let b a b v ~r =
          let open FormulaScored in
          let x_1 = scope a.x ~scope:`Body1 in
          let x_2 = scope b.x ~scope:`Body2 in
          let y_1 = scope a.y ~scope:`Body1 in
          let y_2 = scope b.y ~scope:`Body2 in
          let r_1 = scope r ~scope:`Body1 in
          let r_2 = scope r ~scope:`Body2 in
          sqr (x_2 - x_1) + sqr (y_2 - y_1) - sqr (r_1 + r_2)
          |> Formula.to_polynomial ~values:v ~scoped_values:(function
                 | `Body1 -> a.values
                 | `Body2 -> b.values)
        ;;

        let a () =
          let a_vec = vector_var "a_vec" in
          let a_x = vector_x a_vec in
          let a_y = vector_y a_vec in
          let v0_vec = vector_var "v0_vec" in
          let v0_x = vector_x v0_vec in
          let v0_y = vector_y v0_vec in
          let x0 = scalar_var "x0" in
          let y0 = scalar_var "y0" in
          let r = scalar_var "r" in
          let half = scalar_const N.(one / (one + one)) in
          let g = scalar_global "g" in
          let x = Map.of_alist_exn (module Int) [ 0, x0; 1, v0_x; 2, a_x * half ] in
          let y = Map.of_alist_exn (module Int) [ 0, y0; 1, v0_y; 2, a_y * half ] in
          let b1 = { id = 1; values = assert false; x; y } in
          let b2 = { id = 1; values = assert false; x; y } in
          let bd = [ b1; b2 ] in
          let values = assert false in
          let r = Map.of_alist_exn (module Int) [ 0, r ] in
          let x1 = scope b1.x `B1 in
          let x2 = scope b2.x `B2 in
          let sum = FormulaScored.(x1 + x2) in
          bd
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