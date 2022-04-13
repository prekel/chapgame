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
    (N : Module_types.Number) :
  S with type key = Key.t and type scope = Scope.t and type scalar = N.t