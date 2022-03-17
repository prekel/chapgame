open Core

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
     ;;

      (* let rec calcmap
          : type underlying result.
            (string, underlying, String.comparator_witness) Map.t
            -> (underlying, result) t
            -> result
        =
       fun u -> function
        | ScalarConst (_, c) -> c
        | VectorConst (_, c1, c2) -> c1, c2
        | Scalar name -> Map.find_exn u name
        | Vector name -> Map.find_exn u name
        | VecOfScalars (l, r) ->
          let ul, ur = u, u in
          calcmap ul l, calc ur r
        | Sum (l, r) ->
          let ul, ur = u, u in
          Float.(calc ul l + calc ur r)
        | Mult (l, r) ->
          let ul, ur = u, u in
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
     ;; *)

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
  type t = GiveVelocity of Time_ns.Span.t * Figure.Id.t * Figure.Velocity.t Figure.Vec.t
end

module Model = struct
  type t = (Time_ns.Span.t, Scene.t, Time_ns.Span.comparator_witness) Map.t

  let e () : t = Map.empty (module Time_ns.Span)
end

module Engine = struct
  let recv model action = model, assert false
end
