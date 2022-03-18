open Core

module Make (N : Module_types.Number) = struct
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
      module CoefValue = struct
        type _ t =
          | Scalar : float -> float t
          | Vector : (float * float) -> (float * float) t
      end

      module CoefficientVar = struct
        type ('underlying, 'result) t =
          | ScalarConst : string * float -> (unit, float) t
          | VectorConst : string * float * float -> (unit, float * float) t
          | Scalar : string -> (float, float) t
          | Vector : string -> (float * float, float * float) t
          | VecOfScalars : (('a, float) t * ('b, float) t) -> ('a * 'b, float * float) t
          | Sum : ('a, float) t * ('b, float) t -> ('a * 'b, float) t
          | Mult : ('a, float) t * ('b, float) t -> ('a * 'b, float) t
          | VecX : ('a, float * float) t -> ('a, float) t
          | VecY : ('a, float * float) t -> ('a, float) t
          | VecLen : ('a, float * float) t -> ('a, float) t
          | VecNormalized : ('a, float * float) t -> ('a, float * float) t
          | VecMultScalar :
              ('a, float * float) t * ('b, float) t
              -> ('a * 'b, float * float) t
          | VecInverted : ('a, float * float) t -> ('a, float * float) t
          | Fix : ('a, 'b) t * ('c -> 'a) * ('b -> 'd) -> ('c, 'd) t

        let _ = Scalar ""

        let rec calc
            : type underlying result. underlying -> (underlying, result) t -> result
          =
         fun u -> function
          | ScalarConst (_, c) -> c
          | VectorConst (_, c1, c2) -> c1, c2
          | Scalar _ -> u
          | Vector _ -> u
          | VecOfScalars (l, r) ->
            let ul, ur = u in
            calc ul l, calc ur r
          | Sum (l, r) ->
            let ul, ur = u in
            Float.(calc ul l + calc ur r)
          | Mult (l, r) ->
            let ul, ur = u in
            Float.(calc ul l * calc ur r)
          | VecX e ->
            let x, _y = calc u e in
            x
          | VecY e ->
            let _, y = calc u e in
            y
          | VecLen e ->
            let x, y = calc u e in
            Float.(sqrt ((x ** 2.) + (y ** 2.)))
          | VecNormalized e ->
            let x, y = calc u e in
            let len = Float.(sqrt ((x ** 2.) + (y ** 2.))) in
            Float.(x / len, y / len)
          | VecMultScalar (a, b) ->
            let ul, ur = u in
            let x, y = calc ul a in
            let z = calc ur b in
            Float.(x * z, y * z)
          | VecInverted e ->
            let x, y = calc u e in
            Float.(-x, -y)
          | Fix (i, f1, f2) -> f2 @@ calc (f1 u) i
          | _ -> assert false
       ;;

        (* let rec calcmap : type underlying result. (string, underlying,
           String.comparator_witness) Map.t -> (underlying, result) t -> result = fun u ->
           function | ScalarConst (_, c) -> c | VectorConst (_, c1, c2) -> c1, c2 | Scalar
           name -> Map.find_exn u name | Vector name -> Map.find_exn u name | VecOfScalars
           (l, r) -> let ul, ur = u, u in calcmap ul l, calc ur r | Sum (l, r) -> let ul,
           ur = u, u in Float.(calc ul l + calc ur r) | Mult (l, r) -> let ul, ur = u, u
           in Float.(calc ul l * calc ur r) | VecX e -> let x, _y = calc u e in x | VecY e
           -> let _, y = calc u e in y | VecLen e -> let x, y = calc u e in Float.(sqrt
           ((x ** 2.) + (y ** 2.))) | VecNormalized e -> let x, y = calc u e in let len =
           Float.(sqrt ((x ** 2.) + (y ** 2.))) in Float.(x / len, y / len) |
           VecMultScalar (a, b) -> let ul, ur = u in let x, y = calc ul a in let z = calc
           ur b in Float.(x * z, y * z) | VecInverted e -> let x, y = calc u e in
           Float.(-x, -y) | Fix (i, f1, f2) -> f2 @@ calc (f1 u) i ;; *)

        module Infix = struct
          let ( + ) a b = Sum (a, b)
          let ( * ) a b = Sum (a, b)
          let normalize a = VecNormalized a
          let scalar ~name = Scalar name
          let const ~name v = ScalarConst (name, v)
          let fix i ~f1 ~f2 = Fix (i, f1, f2)

          let tr =
            let a = scalar ~name:"a" in
            let b = scalar ~name:"b" in
            let pi = const ~name:"pi" 3.14 in
            let res = (a + b) * pi |> fix ~f1:(fun (a, b) -> (a, b), ()) ~f2:Fn.id in
            fun ~a ~b -> calc (a, b) res
          ;;

          let tr =
            let a = scalar ~name:"a" in
            let b = scalar ~name:"b" in
            let pi = const ~name:"pi" 3.14 in
            let res = (a + b) * pi in
            fun ~a ~b -> calc ((a, b), ()) res
          ;;
        end

        let c () = calc (1., 3.) (Vector "")
        let c () = calc 2. (Scalar "")
        let c () = calc (1., 3.) (VecX (Vector ""))
        let v0_vec = Vector "v0_vec"
        let v0_x = VecX v0_vec
        let v0_y = VecY v0_vec
        let sum () = calc ((2., 3.), 1.) (VecMultScalar (v0_vec, Scalar ""))
        let g = Scalar "g"
        let mu = Scalar "mu"
        let a_abs = Mult (g, mu)
        let a_vec = VecMultScalar (VecInverted (VecNormalized v0_vec), a_abs)
        let a_x = VecX a_vec
        let a_y = VecY a_vec
        let calcax ~g ~mu ~v0_vec = calc ((g, mu), v0_vec) a_x
        let calcay ~g ~mu ~v0_vec = calc ((g, mu), v0_vec) a_y
      end

      module Var = struct
        type t =
          | Scalar
          | Vector
      end

      module Expr = struct
        type scalar_key = string
        type vector_key = string
        type scalar_values = (scalar_key, float, String.comparator_witness) Map.t
        type vector_values = (vector_key, float * float, String.comparator_witness) Map.t
        type scalar = float
        type vector = float * float
        type monomial = scalar
        type polynomial = (int, monomial, Int.comparator_witness) Map.t

        type _ t =
          | ScalarVar : scalar_key -> scalar t
          | VectorVar : vector_key -> vector t
          | Sum : 'a t * 'a t -> 'a t
          | Mult : 'a t * 'a t -> 'a t
          | Inv : 'a t -> 'a t
          | XOfVector : vector t -> scalar t
          | YOfVector : vector t -> scalar t
          | LengthOfVector : vector t -> scalar t
          | Monomial : scalar * scalar t -> monomial t
          | Polynomial : (int, monomial t, Int.comparator_witness) Map.t -> polynomial t

        let _ =
          Polynomial (Map.of_alist_exn (module Int) [ 1, Monomial (-2., ScalarVar "x0") ])
        ;;

        module type ScalarVec = sig
          type t

          val ( + ) : t -> t -> t
          val ( ~- ) : t -> t
        end

        module FS = Float

        module FV = struct
          open Float

          type t = float * float

          let ( + ) (x1, y1) (x2, y2) = x1 + x2, y1 + y2
          let ( ~- ) (x, y) = -x, -y
        end

        module FP = struct
          type t = polynomial

          let ( + ) = Map.merge_skewed ~combine:(fun ~key:_ a b -> Float.(a + b))
          let ( ~- ) = Map.map ~f:(fun v -> Float.(-v))

          (* let ( * ) a b = Map.map a ~f:(fun v1 -> Map.map b ~f:(fun v2 -> v1 * v2)) *)
          let ( * ) a b =
            Map.fold
              a
              ~init:(Map.empty (module Int))
              ~f:(fun ~key:degree1 ~data:coef1 acc ->
                Map.merge_skewed
                  acc
                  (Map.map b ~f:(fun coef2 -> Float.(coef1 * coef2))
                  |> Map.map_keys_exn (module Int) ~f:(fun degree2 -> degree1 * degree2))
                  ~combine:(fun ~key:_ v1 v2 -> Float.(v1 + v2)))
          ;;
        end

        let rec calc
            : type result.
              scalar_values
              -> vector_values
              -> (module ScalarVec with type t = result)
              -> result t
              -> result
          =
         fun scalar_values vector_values (module ScalarVec) ->
          let open ScalarVec in
          function
          | ScalarVar name -> Map.find_exn scalar_values name
          | VectorVar name -> Map.find_exn vector_values name
          | Sum (a, b) ->
            let calc = calc scalar_values vector_values (module ScalarVec) in
            let ca = calc a in
            let cb = calc b in
            ca + cb
          | Mult _ -> assert false
          | Inv _ -> assert false
          | XOfVector _ -> assert false
          | YOfVector _ -> assert false
          | LengthOfVector _ -> assert false
          | Monomial (c, v) ->
            let q = calc scalar_values vector_values (module Float) v in
            Float.(c * q)
          | Polynomial map ->
            Map.map map ~f:(fun m -> calc scalar_values vector_values (module Float) m)
       ;;

        module Sample = struct
          let a_vec = VectorVar "a_vec"
          let a_x = XOfVector a_vec
          let a_y = YOfVector a_vec
          let v0_vec = VectorVar "v0_vec"
          let v0_x = XOfVector v0_vec
          let v0_y = YOfVector v0_vec
          let x0 = ScalarVar "x0"
          let y0 = ScalarVar "y0"

          let x =
            Polynomial
              (Map.of_alist_exn
                 (module Int)
                 [ 0, Monomial (1., x0); 1, Monomial (1., v0_x); 2, Monomial (0.5, a_x) ])
          ;;

          let y =
            Polynomial
              (Map.of_alist_exn
                 (module Int)
                 [ 0, Monomial (1., y0); 1, Monomial (1., v0_y); 2, Monomial (0.5, a_y) ])
          ;;

          let r = ScalarVar "r"
          let xxyy = Sum (Mult (x, x), Mult (y, y))
        end
      end

      module Expr1 = struct
        type t = (int, Expr.scalar Expr.t, Int.comparator_witness) Map.t

        let ( + ) = Map.merge_skewed ~combine:(fun ~key:_ a b -> Float.(Expr.Sum (a, b)))
        let ( ~- ) = Map.map ~f:(fun v -> Float.(-v))

        (* let ( * ) a b = Map.map a ~f:(fun v1 -> Map.map b ~f:(fun v2 -> v1 * v2)) *)
        let ( * ) a b =
          Map.fold
            a
            ~init:(Map.empty (module Int))
            ~f:(fun ~key:degree1 ~data:coef1 acc ->
              Map.merge_skewed
                acc
                (Map.map b ~f:(fun coef2 -> Expr.Mult (coef1, coef2))
                |> Map.map_keys_exn (module Int) ~f:(fun degree2 -> degree1 * degree2))
                ~combine:(fun ~key:_ v1 v2 -> Expr.Sum (v1, v2)))
        ;;

        let (_ : t) = Map.of_alist_exn (module Int) [ 1, Expr.ScalarVar "" ]
        let to_polynomial s v = Map.map ~f:(fun a -> Expr.calc s v (module Expr.FS) a)
      end

      type t = (int, float, Int.comparator_witness) Map.t
    end

    type t =
      { id : int
      ; vars : (string, float, String.comparator_witness) Map.t
      ; x : Formula.t
      }
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
    let recv model action = model, assert false
  end
end