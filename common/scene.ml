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
  end

  module Figure2 = struct
    module Formula = struct
      module VarId = struct
        type t =
          | Global of string
          | Body of string
      end

      module Var = struct
        type key = string [@@deriving sexp]
        type scalar = N.t [@@deriving sexp]
        type vector = N.t * N.t [@@deriving sexp]

        type value =
          | Scalar of scalar
          | Vector of vector
        [@@deriving sexp]

        type scope_id = int [@@deriving sexp]

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

        let sexp_of_values v = [%sexp (Map.to_alist v : (key * value) list)]

        type 'result t =
          | ScalarConst : scalar -> scalar t
          | VectorConst : vector -> vector t
          | ScalarNegInf : scalar t
          | ScalarPosInf : scalar t
          | ScalarZero : scalar t
          | ScalarGlobalVar : key -> scalar t
          | VectorGlobalVar : key -> vector t
          | ScalarVar : key -> scalar t
          | VectorVar : key -> vector t
          | Sum : 'a t * 'a t -> 'a t
          | SumList : 'a t list -> 'a t
          | Sub : 'a t * 'a t -> 'a t
          | Sqr : 'a t -> 'a t
          | Mult : 'a t * 'a t -> 'a t
          | Div : 'a t * 'a t -> 'a t
          | Neg : 'a t -> 'a t
          | XOfVector : vector t -> scalar t
          | YOfVector : vector t -> scalar t
          | LengthOfVector : vector t -> scalar t
          | Scope : scope_id * 'a t -> 'a t

        let rec sexp_of_t : type result. result t -> Sexp.t =
          let sexp a = sexp_of_t a in
          let open Sexp in
          function
          | ScalarConst s -> List [ Atom "ScalarConst"; [%sexp (s : scalar)] ]
          | VectorConst v -> List [ Atom "VectorConst"; [%sexp (v : vector)] ]
          | ScalarNegInf -> List [ Atom "ScalarNegInf" ]
          | ScalarPosInf -> List [ Atom "ScalarPosInf" ]
          | ScalarZero -> List [ Atom "ScalarZero" ]
          | ScalarGlobalVar s -> List [ Atom "ScalarGlobalVar"; [%sexp (s : key)] ]
          | VectorGlobalVar v -> List [ Atom "VectorGlobalVar"; [%sexp (v : key)] ]
          | ScalarVar s -> List [ Atom "ScalarVar"; [%sexp (s : key)] ]
          | VectorVar v -> List [ Atom "VectorVar"; [%sexp (v : key)] ]
          | Sum (a, b) -> List [ Atom "Sum"; sexp a; sexp b ]
          | SumList l -> List [ Atom "SumList"; List (List.map l ~f:sexp) ]
          | Sub (a, b) -> List [ Atom "Sub"; sexp a; sexp b ]
          | Sqr a -> List [ Atom "Sqr"; sexp a ]
          | Mult (a, b) -> List [ Atom "Mult"; sexp a; sexp b ]
          | Div (a, b) -> List [ Atom "Div"; sexp a; sexp b ]
          | Neg a -> List [ Atom "Neg"; sexp a ]
          | XOfVector v -> List [ Atom "XOfVector"; sexp v ]
          | YOfVector v -> List [ Atom "YOfVector"; sexp v ]
          | LengthOfVector v -> List [ Atom "LengthOfVector"; sexp v ]
          | Scope (s, v) -> List [ Atom "Scope"; [%sexp (s : scope_id)]; sexp v ]
        ;;

        module type ScalarVec = sig
          type t

          val zero : t
          val ( + ) : t -> t -> t
          val ( * ) : t -> t -> t
          val ( / ) : t -> t -> t
          val ( ~- ) : t -> t
        end

        module FV = struct
          open N

          type t = N.t * N.t

          let zero = N.zero, N.zero
          let ( + ) (x1, y1) (x2, y2) = x1 + x2, y1 + y2
          let ( * ) (x1, y1) (x2, y2) = x1 * x2, y1 * y2
          let ( / ) (x1, y1) (x2, y2) = x1 / x2, y1 / y2
          let ( ~- ) (x, y) = -x, -y
        end

        let rec calc
            : type result.
              values:values
              -> global_values:values
              -> scoped_values:(scope_id -> values)
              -> (module ScalarVec with type t = result)
              -> result t
              -> result
          =
         fun ~values ~global_values ~scoped_values (module ScalarVec) -> function
          | ScalarConst x -> x
          | VectorConst x -> x
          | ScalarNegInf -> N.neg_infinity
          | ScalarPosInf -> N.infinity
          | ScalarZero -> N.zero
          | ScalarVar name -> Map.find_exn values name |> scalar_exn
          | VectorVar name -> Map.find_exn values name |> vector_exn
          | ScalarGlobalVar name -> Map.find_exn global_values name |> scalar_exn
          | VectorGlobalVar name -> Map.find_exn global_values name |> vector_exn
          | Sum (a, b) ->
            let calc = calc ~values ~global_values ~scoped_values (module ScalarVec) in
            let ca = calc a in
            let cb = calc b in
            ScalarVec.(ca + cb)
          | SumList l ->
            let calc = calc ~values ~global_values ~scoped_values (module ScalarVec) in
            let c = List.sum (module ScalarVec) l ~f:calc in
            c
          | Sub (a, b) ->
            let calc = calc ~values ~global_values ~scoped_values (module ScalarVec) in
            let ca = calc a in
            let cb = calc b in
            ScalarVec.(ca + -cb)
          | Sqr a ->
            let ca = calc ~values ~global_values ~scoped_values (module ScalarVec) a in
            ScalarVec.(ca * ca)
          | Mult (a, b) ->
            let calc = calc ~values ~global_values ~scoped_values (module ScalarVec) in
            let ca = calc a in
            let cb = calc b in
            ScalarVec.(ca * cb)
          | Div (a, b) ->
            let calc = calc ~values ~global_values ~scoped_values (module ScalarVec) in
            let ca = calc a in
            let cb = calc b in
            ScalarVec.(ca / cb)
          | Neg a ->
            let ca = calc ~values ~global_values ~scoped_values (module ScalarVec) a in
            ScalarVec.(-ca)
          | XOfVector v ->
            let x, _y = calc ~values ~global_values ~scoped_values (module FV) v in
            x
          | YOfVector v ->
            let _x, y = calc ~values ~global_values ~scoped_values (module FV) v in
            y
          | LengthOfVector v ->
            let x, y = calc ~values ~global_values ~scoped_values (module FV) v in
            N.(sqrt ((x * x) + (y * y)))
          | Scope (scope, x) ->
            calc
              ~values:(scoped_values scope)
              ~global_values
              ~scoped_values
              (module ScalarVec)
              x
       ;;

        module Infix = struct
          let scalar_var s = ScalarVar s, s
          let vector_var v = VectorVar v, v
          let scalar_const s = ScalarConst s
          let vector_const v = VectorConst v
          let scalar_global s = ScalarGlobalVar s, s
          let vector_global v = VectorGlobalVar v, v
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

      type t = (int, Var.scalar Var.t, Int.comparator_witness) Map.t

      let sexp_of_t (t : t) =
        [%sexp
          (Map.to_alist t |> List.map ~f:(fun (d, v) -> d, Var.sexp_of_t v)
            : (int * Sexp.t) list)]
      ;;

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

      let to_polynomial p ~values ~global_values ~scoped_values : Solver.Polynomial.t =
        Map.map p ~f:(fun a ->
            Var.calc ~values ~global_values ~scoped_values (module N) a)
        |> Solver.Polynomial.of_map
      ;;
    end

    type formula =
      { interval : Formula.Var.scalar Formula.Var.t * Formula.Var.scalar Formula.Var.t
      ; x : Formula.t
      ; y : Formula.t
      }

    type t =
      { id : int
      ; values : Formula.Var.values
      ; x : Formula.t
      ; y : Formula.t (* ; xy : 'a formula list *)
      }
    [@@deriving sexp_of]

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
      let sqr a =
        Map.to_sequence a
        |> Sequence.cartesian_product (Map.to_sequence a)
        |> Sequence.map ~f:(fun ((d1, c1), (d2, c2)) ->
               Int.(d1 + d2), Formula.Var.Mult (c1, c2))
        |> Map.of_sequence_multi (module Int)
        |> Map.map ~f:(fun a -> Formula.Var.SumList a)
      ;;
    end

    module Sample = struct
      type 'a scope =
        [< `Body1 | `Body2 | `Global | `Phantom > `Body1 `Body2 `Phantom ] as 'a

      module Vars = struct
        open Formula.Var.Infix

        let a_vec, _ = vector_var "a_vec"
        let a_x = vector_x a_vec
        let a_y = vector_y a_vec
        let v0_vec, _ = vector_var "v0_vec"
        let v0_x = vector_x v0_vec
        let v0_y = vector_y v0_vec
        let x0, _ = scalar_var "x0"
        let y0, _ = scalar_var "y0"
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
          let x_1 = scope a.x ~scope:1 in
          let x_2 = scope b.x ~scope:2 in
          let y_1 = scope a.y ~scope:1 in
          let y_2 = scope b.y ~scope:2 in
          let r_1 = scope r ~scope:1 in
          let r_2 = scope r ~scope:2 in
          sqr (x_2 - x_1) + sqr (y_2 - y_1) - sqr (r_1 + r_2)
          |> Formula.to_polynomial ~values:v ~global_values:v ~scoped_values:(function
                 | 1 -> a.values
                 | 2 -> b.values
                 | _ -> v)
        ;;

        let b1 a b v ~r =
          let open FormulaScored in
          let x_1 = scope a.x ~scope:1 in
          let x_2 = scope b.x ~scope:2 in
          let y_1 = scope a.y ~scope:1 in
          let y_2 = scope b.y ~scope:2 in
          let r_1 = scope r ~scope:1 in
          let r_2 = scope r ~scope:2 in
          sqr (x_2 - x_1) + sqr (y_2 - y_1) - sqr (r_1 + r_2)
        ;;

        (* let a () = let a_vec, _ = vector_var "a_vec" in let a_x = vector_x a_vec in let
           a_y = vector_y a_vec in let v0_vec, _ = vector_var "v0_vec" in let v0_x =
           vector_x v0_vec in let v0_y = vector_y v0_vec in let x0, _ = scalar_var "x0" in
           let y0, _ = scalar_var "y0" in let r = scalar_var "r" in let half =
           scalar_const N.(one / (one + one)) in let g = scalar_global "g" in let x =
           Map.of_alist_exn (module Int) [ 0, x0; 1, v0_x; 2, a_x * half ] in let y =
           Map.of_alist_exn (module Int) [ 0, y0; 1, v0_y; 2, a_y * half ] in let b1 = {
           id = 1; values = assert false; x; y } in let b2 = { id = 1; values = assert
           false; x; y } in let bd = [ b1; b2 ] in let values = assert false in let r =
           Map.of_alist_exn (module Int) [ 0, r ] in b1, b2 ;;

           let qwqw b1 b2 = let x1 = scope b1.x ~scope:`B1 in let x2 = scope b2.x
           ~scope:`B2 in let sum = FormulaScored.(x1 + x2) in sum ;;

           let _ = let b1, b2 = a () in let s = qwqw b1 b2 in s ;; *)
      end
    end
  end

  module Scene = struct
    type t =
      { figures : (int, Figure2.t, Int.comparator_witness) Map.t
      ; global_values : Figure2.Formula.Var.values
      }

    let figures { figures; _ } = figures

    module Vars1 = struct
      open Figure2.Formula.Var.Infix

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
      let g, g_name = scalar_global "g"
    end

    module Formulas1 = struct
      open Vars1
      open Figure2.Formula.Var.Infix

      let x = Map.of_alist_exn (module Int) [ 0, x0; 1, v0_x; 2, a_x * half ]
      let y = Map.of_alist_exn (module Int) [ 0, y0; 1, v0_y; 2, a_y * half ]
    end

    let scene () =
      let open Figure2.Formula.Var.Infix in
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
      let _g, _g_name = scalar_global "g" in
      let x = Map.of_alist_exn (module Int) [ 0, x0; 1, v0_x; 2, a_x * half ] in
      let y = Map.of_alist_exn (module Int) [ 0, y0; 1, v0_y; 2, a_y * half ] in
      { figures =
          Map.of_alist_exn
            (module Int)
            [ ( 0
              , { Figure2.id = 0
                ; values =
                    Map.of_alist_exn
                      (module String)
                      [ a_vec_name, Figure2.Formula.Var.Vector N.(one, -one / (one + one))
                      ; ( v0_vec_name
                        , Figure2.Formula.Var.Vector N.(-(one + one), one / (one + one)) )
                      ; x0_name, Figure2.Formula.Var.Scalar N.(-(one + one + one))
                      ; y0_name, Figure2.Formula.Var.Scalar N.(one + one + one)
                      ; r_name, Figure2.Formula.Var.Scalar N.(one + one + one + one)
                      ]
                ; x
                ; y (* ; xy = [] *)
                } )
            ; ( 1
              , { Figure2.id = 1
                ; values =
                    Map.of_alist_exn
                      (module String)
                      [ a_vec_name, Figure2.Formula.Var.Vector N.(one, one / (one + one))
                      ; ( v0_vec_name
                        , Figure2.Formula.Var.Vector N.(one + one, one / (one + one)) )
                      ; x0_name, Figure2.Formula.Var.Scalar N.(one + one + one)
                      ; y0_name, Figure2.Formula.Var.Scalar N.(one + one + one)
                      ; r_name, Figure2.Formula.Var.Scalar N.(one)
                      ]
                ; x
                ; y (* ; xy = [] *)
                } )
            ]
      ; global_values = Map.empty (module String)
      }
    ;;

    let t scene =
      let scene = scene in
      let seq = Map.to_sequence scene.figures in
      let p = Sequence.cartesian_product seq seq in
      let r, _ = Figure2.Formula.Var.Infix.scalar_var "r" in
      let r = Map.of_alist_exn (module Int) [ 0, r ] in
      let q =
        Sequence.map p ~f:(fun ((_id1, f1), (_id2, f2)) ->
            Figure2.Sample.Formulas.b f1 f2 scene.global_values ~r)
      in
      q
    ;;

    let t1 scene =
      let scene = scene in
      let seq = Map.to_sequence scene.figures in
      let p = Sequence.cartesian_product seq seq in
      let r, _ = Figure2.Formula.Var.Infix.scalar_var "r" in
      let r = Map.of_alist_exn (module Int) [ 0, r ] in
      let q =
        Sequence.map p ~f:(fun ((_id1, f1), (_id2, f2)) ->
            Figure2.Sample.Formulas.b1 f1 f2 scene.global_values ~r)
      in
      q
    ;;

    let a = t (scene ())
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

    let e () : t =
      Map.of_alist_exn (module Time_ns.Span) [ Time_ns.Span.zero, Scene.scene () ]
    ;;
  end

  module Engine = struct
    let recv model _action = model, assert false
  end
end