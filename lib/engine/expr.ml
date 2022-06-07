open Core
include Expr_intf

module Make
    (N : Solver.Module_types.NUMBER)
    (Var : Module_types.VAR)
    (Scope : Module_types.SCOPE) =
struct
  module N = N
  module Var = Var
  module Scope = Scope

  type key = Var.t [@@deriving sexp, equal]
  type scalar = N.t [@@deriving sexp, equal]
  type vector = N.t * N.t [@@deriving sexp, equal]
  type scope = Scope.t [@@deriving sexp, equal]

  module VectorOps = Vector.Make (N)

  type 'result t =
    | ScalarConst : scalar -> scalar t
    | VectorConst : vector -> vector t
    | ScalarNegInf : scalar t
    | ScalarPosInf : scalar t
    | ScalarZero : scalar t
    | ScalarVar : key -> scalar t
    | VectorVar : key * key -> vector t
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
    | UnitVector : vector t -> vector t
    | VectorOfXY : scalar t * scalar t -> vector t
    | Scope : scope * 'a t -> 'a t

  let rec equal : type result. result t -> result t -> bool =
   fun a b ->
    match a, b with
    | ScalarConst a, ScalarConst b -> N.(a = b)
    | VectorConst (ax, ay), VectorConst (bx, by) -> N.(ax = bx) && N.(ay = by)
    | ScalarNegInf, ScalarNegInf | ScalarPosInf, ScalarPosInf | ScalarZero, ScalarZero ->
      true
    | ScalarVar a, ScalarVar b -> Var.(equal a b)
    | VectorVar (a_x, a_y), VectorVar (b_x, b_y) -> Var.(equal a_x b_x && equal a_y b_y)
    | Sum (al, ar), Sum (bl, br)
    | Sub (al, ar), Sub (bl, br)
    | Mult (al, ar), Mult (bl, br)
    | Div (al, ar), Div (bl, br) -> equal al bl && equal ar br
    | SumList a, SumList b -> List.equal equal a b
    | Sqr a, Sqr b | Neg a, Neg b -> equal a b
    | XOfVector a, XOfVector b
    | YOfVector a, YOfVector b
    | LengthOfVector a, LengthOfVector b -> equal a b
    | UnitVector a, UnitVector b -> equal a b
    | VectorOfXY (al, ar), VectorOfXY (bl, br) -> equal al bl && equal ar br
    | Scope (sa, a), Scope (sb, b) -> Scope.(equal sa sb) && equal a b
    | _ -> false
 ;;

  type t_scalar = scalar t
  type t_vector = vector t

  let equal_t_scalar = equal
  let equal_t_vector = equal

  type trig_op =
    [ `Cos
    | `Sin
    ]
  [@@deriving sexp, equal]

  let rec t_scalar_of_sexp = function
    | Sexp.List [ Atom "ScalarConst"; s ] -> ScalarConst (scalar_of_sexp s)
    | Sexp.List [ Atom "ScalarNegInf" ] -> ScalarNegInf
    | Sexp.List [ Atom "ScalarPosInf" ] -> ScalarPosInf
    | Sexp.List [ Atom "ScalarZero" ] -> ScalarZero
    | Sexp.List [ Atom "ScalarVar"; key ] -> ScalarVar (key_of_sexp key)
    | Sexp.List [ Atom "Sum"; a; b ] -> Sum (t_scalar_of_sexp a, t_scalar_of_sexp b)
    | Sexp.List [ Atom "SumList"; l ] -> SumList (List.t_of_sexp t_scalar_of_sexp l)
    | Sexp.List [ Atom "Sub"; a; b ] -> Sub (t_scalar_of_sexp a, t_scalar_of_sexp b)
    | Sexp.List [ Atom "Sqr"; a ] -> Sqr (t_scalar_of_sexp a)
    | Sexp.List [ Atom "Mult"; a; b ] -> Mult (t_scalar_of_sexp a, t_scalar_of_sexp b)
    | Sexp.List [ Atom "Div"; a; b ] -> Div (t_scalar_of_sexp a, t_scalar_of_sexp b)
    | Sexp.List [ Atom "Neg"; a ] -> Neg (t_scalar_of_sexp a)
    | Sexp.List [ Atom "XOfVector"; v ] -> XOfVector (t_vector_of_sexp v)
    | Sexp.List [ Atom "YOfVector"; v ] -> YOfVector (t_vector_of_sexp v)
    | Sexp.List [ Atom "LengthOfVector"; v ] -> LengthOfVector (t_vector_of_sexp v)
    | Sexp.List [ Atom "Scope"; s; v ] -> Scope (Scope.t_of_sexp s, t_scalar_of_sexp v)
    | other -> Error.raise_s [%message "Cannot deserialize" ~sexp:(other : Sexp.t)]

  and t_vector_of_sexp = function
    | Sexp.List [ Atom "VectorConst"; v ] -> VectorConst (vector_of_sexp v)
    | Sexp.List [ Atom "VectorVar"; x; y ] -> VectorVar (key_of_sexp x, key_of_sexp y)
    | Sexp.List [ Atom "Sum"; a; b ] -> Sum (t_vector_of_sexp a, t_vector_of_sexp b)
    | Sexp.List [ Atom "SumList"; l ] -> SumList (List.t_of_sexp t_vector_of_sexp l)
    | Sexp.List [ Atom "Sub"; a; b ] -> Sub (t_vector_of_sexp a, t_vector_of_sexp b)
    | Sexp.List [ Atom "Sqr"; a ] -> Sqr (t_vector_of_sexp a)
    | Sexp.List [ Atom "Mult"; a; b ] -> Mult (t_vector_of_sexp a, t_vector_of_sexp b)
    | Sexp.List [ Atom "Div"; a; b ] -> Div (t_vector_of_sexp a, t_vector_of_sexp b)
    | Sexp.List [ Atom "Neg"; a ] -> Neg (t_vector_of_sexp a)
    | Sexp.List [ Atom "UnitVector"; a ] -> UnitVector (t_vector_of_sexp a)
    | Sexp.List [ Atom "VectorOfXY"; a; b ] ->
      VectorOfXY (t_scalar_of_sexp a, t_scalar_of_sexp b)
    | Sexp.List [ Atom "Scope"; s; v ] -> Scope (Scope.t_of_sexp s, t_vector_of_sexp v)
    | other -> Error.raise_s [%message "Cannot deserialize" ~sexp:(other : Sexp.t)]
  ;;

  let rec sexp_of_t : type result. result t -> Sexp.t = function
    | ScalarConst s -> List [ Atom "ScalarConst"; [%sexp (s : scalar)] ]
    | VectorConst v -> List [ Atom "VectorConst"; [%sexp (v : vector)] ]
    | ScalarNegInf -> List [ Atom "ScalarNegInf" ]
    | ScalarPosInf -> List [ Atom "ScalarPosInf" ]
    | ScalarZero -> List [ Atom "ScalarZero" ]
    | ScalarVar s -> List [ Atom "ScalarVar"; [%sexp (s : key)] ]
    | VectorVar (x, y) -> List [ Atom "VectorVar"; [%sexp (x : key)]; [%sexp (y : key)] ]
    | Sum (a, b) -> List [ Atom "Sum"; sexp_of_t a; sexp_of_t b ]
    | SumList l -> List [ Atom "SumList"; List (List.map l ~f:sexp_of_t) ]
    | Sub (a, b) -> List [ Atom "Sub"; sexp_of_t a; sexp_of_t b ]
    | Sqr a -> List [ Atom "Sqr"; sexp_of_t a ]
    | Mult (a, b) -> List [ Atom "Mult"; sexp_of_t a; sexp_of_t b ]
    | Div (a, b) -> List [ Atom "Div"; sexp_of_t a; sexp_of_t b ]
    | Neg a -> List [ Atom "Neg"; sexp_of_t a ]
    | XOfVector v -> List [ Atom "XOfVector"; sexp_of_t v ]
    | YOfVector v -> List [ Atom "YOfVector"; sexp_of_t v ]
    | LengthOfVector v -> List [ Atom "LengthOfVector"; sexp_of_t v ]
    | UnitVector v -> List [ Atom "UnitVector"; sexp_of_t v ]
    | VectorOfXY (a, b) -> List [ Atom "VectorOfXY"; sexp_of_t a; sexp_of_t b ]
    | Scope (s, v) -> List [ Atom "Scope"; [%sexp (s : Scope.t)]; sexp_of_t v ]
  ;;

  let sexp_of_t_scalar : t_scalar -> Sexp.t = sexp_of_t
  let sexp_of_t_vector : t_vector -> Sexp.t = sexp_of_t

  let rec calc
      : type result.
        values:(key -> N.t)
        -> scoped_values:(Scope.t -> key -> N.t)
        -> (module Common.Module_types.BASIC_OPS with type t = result)
        -> result t
        -> result
    =
   fun ~values ~scoped_values (module Ops) -> function
    | ScalarConst x -> x
    | VectorConst x -> x
    | ScalarNegInf -> N.neg_infinity
    | ScalarPosInf -> N.infinity
    | ScalarZero -> N.zero
    | ScalarVar name -> values name
    | VectorVar (name_x, name_y) -> values name_x, values name_y
    | Sum (a, b) ->
      let calc = calc ~values ~scoped_values (module Ops) in
      let ca = calc a in
      let cb = calc b in
      Ops.(ca + cb)
    | SumList l ->
      let calc = calc ~values ~scoped_values (module Ops) in
      let c = List.sum (module Ops) l ~f:calc in
      c
    | Sub (a, b) ->
      let calc = calc ~values ~scoped_values (module Ops) in
      let ca = calc a in
      let cb = calc b in
      Ops.(ca - cb)
    | Sqr a ->
      let ca = calc ~values ~scoped_values (module Ops) a in
      Ops.(ca * ca)
    | Mult (a, b) ->
      let calc = calc ~values ~scoped_values (module Ops) in
      let ca = calc a in
      let cb = calc b in
      Ops.(ca * cb)
    | Div (a, b) ->
      let calc = calc ~values ~scoped_values (module Ops) in
      let ca = calc a in
      let cb = calc b in
      Ops.(if equal ca zero && equal cb zero then zero else ca / cb)
    | Neg a ->
      let ca = calc ~values ~scoped_values (module Ops) a in
      Ops.(-ca)
    | XOfVector v ->
      let x, _y = calc ~values ~scoped_values (module VectorOps) v in
      x
    | YOfVector v ->
      let _x, y = calc ~values ~scoped_values (module VectorOps) v in
      y
    | LengthOfVector v ->
      let x, y = calc ~values ~scoped_values (module VectorOps) v in
      N.(sqrt ((x * x) + (y * y)))
    | UnitVector v ->
      let x, y = calc ~values ~scoped_values (module VectorOps) v in
      let length = N.(sqrt ((x * x) + (y * y))) in
      N.(if equal length zero then zero, zero else x / length, y / length)
    | VectorOfXY (a, b) ->
      let calc = calc ~values ~scoped_values (module N) in
      let x = calc a in
      let y = calc b in
      x, y
    | Scope (scope, x) -> calc ~values:(scoped_values scope) ~scoped_values (module Ops) x
 ;;

  module Syntax = struct
    let scalar_var s = ScalarVar s
    let vector_var x y = VectorVar (x, y)
    let scalar_const s = ScalarConst s
    let vector_const v = VectorConst v
    let ( + ) a b = Sum (a, b)
    let ( - ) a b = Sub (a, b)
    let ( * ) a b = Mult (a, b)
    let ( / ) a b = Div (a, b)
    let ( ~- ) a = Neg a
    let sqr a = Sqr a
    let vector_length v = LengthOfVector v
    let vector_x v = XOfVector v
    let vector_y v = YOfVector v
    let vector_unit v = UnitVector v
    let vector_of_scalar a b = VectorOfXY (a, b)
    let scope ~scope a = Scope (scope, a)
  end
end
