module Make (Key : sig
  type t [@@deriving sexp, equal]
end) (Scope : sig
  type t [@@deriving sexp, equal]
end)
(N : Module_types.Number) : sig
  type key = Key.t [@@deriving sexp]
  type scalar = N.t [@@deriving sexp]
  type vector = scalar * scalar [@@deriving sexp]

  type value =
    | Scalar of scalar
    | Vector of vector
  [@@deriving sexp]

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

  val equal : 'result t -> 'result t -> bool
  val sexp_of_t : 'result t -> Sexplib0.Sexp.t

  type t_scalar = scalar t [@@deriving of_sexp]
  type t_vector = vector t [@@deriving of_sexp]

  val calc
    :  values:values
    -> global_values:values
    -> scoped_values:(Scope.t -> values)
    -> (module Module_types.BasicOps with type t = 'result)
    -> 'result t
    -> 'result

  module Syntax : sig
    val scalar_var : key -> scalar t * key
    val vector_var : key -> vector t * key
    val scalar_const : scalar -> scalar t
    val vector_const : vector -> vector t
    val scalar_global : key -> scalar t * key
    val vector_global : key -> vector t * key
    val ( + ) : 'a t -> 'a t -> 'a t
    val ( - ) : 'a t -> 'a t -> 'a t
    val ( * ) : 'a t -> 'a t -> 'a t
    val ( / ) : 'a t -> 'a t -> 'a t
    val ( ~- ) : 'a t -> 'a t
    val vector_length : vector t -> scalar t
    val vector_x : vector t -> scalar t
    val vector_y : vector t -> scalar t
    val scope : scope:Scope.t -> 'a t -> 'a t
  end
end
