open Core

module Make (Key : sig
  type t [@@deriving sexp, equal]
end) (Scope : sig
  type t [@@deriving sexp, equal]
end)
(N : Module_types.Number) =
struct
  type key = Key.t [@@deriving sexp]
  type scalar = N.t [@@deriving sexp]
  type vector = N.t * N.t [@@deriving sexp]

  type value =
    | Scalar of scalar
    | Vector of vector
  [@@deriving sexp]

  let scalar_exn = function
    | Scalar s -> s
    | Vector v -> Error.raise_s [%message "Expected scalar, got vector" ~v:(v : vector)]
  ;;

  let vector_exn = function
    | Scalar s -> Error.raise_s [%message "Expected vector, got scalar" ~s:(s : scalar)]
    | Vector v -> v
  ;;

  type values = key -> value


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
    | Scope : Scope.t * 'a t -> 'a t

  let rec equal : type result. result t -> result t -> bool =
   fun a b ->
    match a, b with
    | ScalarConst a, ScalarConst b -> N.(a = b)
    | VectorConst (ax, ay), VectorConst (bx, by) -> N.(ax = bx) && N.(ay = by)
    | ScalarNegInf, ScalarNegInf | ScalarPosInf, ScalarPosInf | ScalarZero, ScalarZero ->
      true
    | ScalarGlobalVar a, ScalarGlobalVar b
    | VectorGlobalVar a, VectorGlobalVar b
    | ScalarVar a, ScalarVar b
    | VectorVar a, VectorVar b -> Key.(equal a b)
    | Sum (al, ar), Sum (bl, br)
    | Sub (al, ar), Sub (bl, br)
    | Mult (al, ar), Mult (bl, br)
    | Div (al, ar), Div (bl, br) -> equal al bl && equal ar br
    | SumList a, SumList b -> List.equal equal a b
    | Sqr a, Sqr b | Neg a, Neg b -> equal a b
    | XOfVector a, XOfVector b
    | YOfVector a, YOfVector b
    | LengthOfVector a, LengthOfVector b -> equal a b
    | Scope (sa, a), Scope (sb, b) -> Scope.(equal sa sb) && equal a b
    | _ -> false
 ;;

  type t_scalar = scalar t
  type t_vector = vector t

  let rec t_scalar_of_sexp = function
    | Sexp.List [ Atom "ScalarConst"; s ] -> ScalarConst (scalar_of_sexp s)
    | Sexp.List [ Atom "ScalarNegInf" ] -> ScalarNegInf
    | Sexp.List [ Atom "ScalarPosInf" ] -> ScalarPosInf
    | Sexp.List [ Atom "ScalarZero" ] -> ScalarZero
    | Sexp.List [ Atom "ScalarGlobalVar"; key ] -> ScalarGlobalVar (Key.t_of_sexp key)
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
    | Sexp.List [ Atom "VectorGlobalVar"; v ] -> VectorGlobalVar (key_of_sexp v)
    | Sexp.List [ Atom "VectorVar"; v ] -> VectorVar (key_of_sexp v)
    | Sexp.List [ Atom "Sum"; a; b ] -> Sum (t_vector_of_sexp a, t_vector_of_sexp b)
    | Sexp.List [ Atom "SumList"; l ] -> SumList (List.t_of_sexp t_vector_of_sexp l)
    | Sexp.List [ Atom "Sub"; a; b ] -> Sub (t_vector_of_sexp a, t_vector_of_sexp b)
    | Sexp.List [ Atom "Sqr"; a ] -> Sqr (t_vector_of_sexp a)
    | Sexp.List [ Atom "Mult"; a; b ] -> Mult (t_vector_of_sexp a, t_vector_of_sexp b)
    | Sexp.List [ Atom "Div"; a; b ] -> Div (t_vector_of_sexp a, t_vector_of_sexp b)
    | Sexp.List [ Atom "Neg"; a ] -> Neg (t_vector_of_sexp a)
    | Sexp.List [ Atom "Scope"; s; v ] -> Scope (Scope.t_of_sexp s, t_vector_of_sexp v)
    | other -> Error.raise_s [%message "Cannot deserialize" ~sexp:(other : Sexp.t)]
  ;;

  let rec sexp_of_t : type result. result t -> Sexp.t = function
    | ScalarConst s -> List [ Atom "ScalarConst"; [%sexp (s : scalar)] ]
    | VectorConst v -> List [ Atom "VectorConst"; [%sexp (v : vector)] ]
    | ScalarNegInf -> List [ Atom "ScalarNegInf" ]
    | ScalarPosInf -> List [ Atom "ScalarPosInf" ]
    | ScalarZero -> List [ Atom "ScalarZero" ]
    | ScalarGlobalVar s -> List [ Atom "ScalarGlobalVar"; [%sexp (s : key)] ]
    | VectorGlobalVar v -> List [ Atom "VectorGlobalVar"; [%sexp (v : key)] ]
    | ScalarVar s -> List [ Atom "ScalarVar"; [%sexp (s : key)] ]
    | VectorVar v -> List [ Atom "VectorVar"; [%sexp (v : key)] ]
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
    | Scope (s, v) -> List [ Atom "Scope"; [%sexp (s : Scope.t)]; sexp_of_t v ]
  ;;

  module VectorOps : Module_types.BasicOps with type t = vector = struct
    type t = vector

    let zero = N.zero, N.zero

    open N

    let ( + ) (x1, y1) (x2, y2) = x1 + x2, y1 + y2
    let ( - ) (x1, y1) (x2, y2) = x1 - x2, y1 - y2
    let ( * ) (x1, y1) (x2, y2) = x1 * x2, y1 * y2
    let ( / ) (x1, y1) (x2, y2) = x1 / x2, y1 / y2
    let ( ~- ) (x, y) = -x, -y
  end

  let rec calc
      : type result.
        values:values
        -> global_values:values
        -> scoped_values:(Scope.t -> values)
        -> (module Module_types.BasicOps with type t = result)
        -> result t
        -> result
    =
   fun ~values ~global_values ~scoped_values (module Ops) -> function
    | ScalarConst x -> x
    | VectorConst x -> x
    | ScalarNegInf -> N.neg_infinity
    | ScalarPosInf -> N.infinity
    | ScalarZero -> N.zero
    | ScalarVar name -> values name |> scalar_exn
    | VectorVar name -> values name |> vector_exn
    | ScalarGlobalVar name -> global_values name |> scalar_exn
    | VectorGlobalVar name -> global_values name |> vector_exn
    | Sum (a, b) ->
      let calc = calc ~values ~global_values ~scoped_values (module Ops) in
      let ca = calc a in
      let cb = calc b in
      Ops.(ca + cb)
    | SumList l ->
      let calc = calc ~values ~global_values ~scoped_values (module Ops) in
      let c = List.sum (module Ops) l ~f:calc in
      c
    | Sub (a, b) ->
      let calc = calc ~values ~global_values ~scoped_values (module Ops) in
      let ca = calc a in
      let cb = calc b in
      Ops.(ca - cb)
    | Sqr a ->
      let ca = calc ~values ~global_values ~scoped_values (module Ops) a in
      Ops.(ca * ca)
    | Mult (a, b) ->
      let calc = calc ~values ~global_values ~scoped_values (module Ops) in
      let ca = calc a in
      let cb = calc b in
      Ops.(ca * cb)
    | Div (a, b) ->
      let calc = calc ~values ~global_values ~scoped_values (module Ops) in
      let ca = calc a in
      let cb = calc b in
      Ops.(ca / cb)
    | Neg a ->
      let ca = calc ~values ~global_values ~scoped_values (module Ops) a in
      Ops.(-ca)
    | XOfVector v ->
      let x, _y = calc ~values ~global_values ~scoped_values (module VectorOps) v in
      x
    | YOfVector v ->
      let _x, y = calc ~values ~global_values ~scoped_values (module VectorOps) v in
      y
    | LengthOfVector v ->
      let x, y = calc ~values ~global_values ~scoped_values (module VectorOps) v in
      N.(sqrt ((x * x) + (y * y)))
    | Scope (scope, x) ->
      calc ~values:(scoped_values scope) ~global_values ~scoped_values (module Ops) x
 ;;

  module Syntax = struct
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
