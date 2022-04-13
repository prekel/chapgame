open Core

module type S = sig
  type key [@@deriving sexp, equal]
  type scalar [@@deriving sexp, equal]
  type vector = scalar * scalar [@@deriving sexp, equal]
  type scope [@@deriving sexp, equal]

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
    | Trig : [ `Cos | `Sin ] * scalar t -> scalar t
    | VectorAngle : vector t -> scalar t
    | XOfVector : vector t -> scalar t
    | YOfVector : vector t -> scalar t
    | LengthOfVector : vector t -> scalar t
    | UnitVector : vector t -> vector t
    | VectorOfXY : scalar t * scalar t -> vector t
    | Scope : scope * 'a t -> 'a t

  val equal : 'result t -> 'result t -> bool
  val sexp_of_t : 'result t -> Sexplib0.Sexp.t

  type t_scalar = scalar t [@@deriving sexp, equal]
  type t_vector = vector t [@@deriving sexp, equal]

  val calc
    :  values:(key -> scalar)
    -> scoped_values:(scope -> key -> scalar)
    -> (module Module_types.BasicOps with type t = 'result)
    -> 'result t
    -> 'result

  module VectorOps : Module_types.BasicOps with type t = vector

  module Syntax : sig
    val scalar_var : key -> scalar t
    val vector_var : key -> key -> vector t
    val scalar_const : scalar -> scalar t
    val vector_const : vector -> vector t
    val ( + ) : 'a t -> 'a t -> 'a t
    val ( - ) : 'a t -> 'a t -> 'a t
    val ( * ) : 'a t -> 'a t -> 'a t
    val ( / ) : 'a t -> 'a t -> 'a t
    val ( ~- ) : 'a t -> 'a t
    val sqr : 'a t -> 'a t
    val vector_length : vector t -> scalar t
    val vector_x : vector t -> scalar t
    val vector_y : vector t -> scalar t
    val vector_unit : vector t -> vector t
    val vector_of_scalar : scalar t -> scalar t -> vector t
    val scope : scope:scope -> 'a t -> 'a t
    val cos : scalar t -> scalar t
    val sin : scalar t -> scalar t
    val vector_angle : vector t -> scalar t
  end
end

module Make
    (Key : Module_types.Key)
    (Scope : Module_types.Scope)
    (N : Module_types.Number) =
struct
  type key = Key.t [@@deriving sexp, equal]
  type scalar = N.t [@@deriving sexp, equal]
  type vector = N.t * N.t [@@deriving sexp, equal]
  type scope = Scope.t [@@deriving sexp, equal]

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
    | Trig : [ `Cos | `Sin ] * scalar t -> scalar t
    | VectorAngle : vector t -> scalar t
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
    | ScalarVar a, ScalarVar b -> Key.(equal a b)
    | VectorVar (a_x, a_y), VectorVar (b_x, b_y) -> Key.(equal a_x b_x && equal a_y b_y)
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
    | Sexp.List [ Atom "Trig"; op; a ] -> Trig (trig_op_of_sexp op, t_scalar_of_sexp a)
    | Sexp.List [ Atom "VectorAngle"; a ] -> VectorAngle (t_vector_of_sexp a)
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
    | Trig (op, a) -> List [ Atom "Trig"; sexp_of_trig_op op; sexp_of_t a ]
    | VectorAngle v -> List [ Atom "VectorAngle"; sexp_of_t v ]
    | XOfVector v -> List [ Atom "XOfVector"; sexp_of_t v ]
    | YOfVector v -> List [ Atom "YOfVector"; sexp_of_t v ]
    | LengthOfVector v -> List [ Atom "LengthOfVector"; sexp_of_t v ]
    | UnitVector v -> List [ Atom "UnitVector"; sexp_of_t v ]
    | VectorOfXY (a, b) -> List [ Atom "VectorOfXY"; sexp_of_t a; sexp_of_t b ]
    | Scope (s, v) -> List [ Atom "Scope"; [%sexp (s : Scope.t)]; sexp_of_t v ]
  ;;

  let sexp_of_t_scalar : t_scalar -> Sexp.t = sexp_of_t
  let sexp_of_t_vector : t_vector -> Sexp.t = sexp_of_t

  module VectorOps : Module_types.BasicOps with type t = vector = struct
    type t = vector [@@deriving equal]

    let zero = N.zero, N.zero

    open N

    let ( + ) (x1, y1) (x2, y2) = x1 + x2, y1 + y2
    let ( - ) (x1, y1) (x2, y2) = x1 - x2, y1 - y2
    let ( * ) (x1, y1) (x2, y2) = x1 * x2, y1 * y2
    let ( / ) (x1, y1) (x2, y2) = x1 / x2, y1 / y2
    let ( ~- ) (x, y) = -x, -y

    let sin, cos, atan2 =
      (fun _ -> assert false), (fun _ -> assert false), fun _ -> assert false
    ;;
  end

  let rec calc
      : type result.
        values:(key -> N.t)
        -> scoped_values:(Scope.t -> key -> N.t)
        -> (module Module_types.BasicOps with type t = result)
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
    | Trig (op, a) ->
      let x = calc ~values ~scoped_values (module N) a in
      begin
        match op with
        | `Cos -> N.cos x
        | `Sin -> N.sin x
      end
    | VectorAngle v ->
      let x, y = calc ~values ~scoped_values (module VectorOps) v in
      N.(atan2 y x)
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
    let cos a = Trig (`Cos, a)
    let sin a = Trig (`Sin, a)
    let vector_angle v = VectorAngle v
  end
end