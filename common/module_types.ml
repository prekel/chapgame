open Core

module type BasicOps = sig
  type t

  val zero : t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( ~- ) : t -> t
  val sin : t -> t
  val cos : t -> t
  val atan2 : t -> t -> t
end

module type Number = sig
  type t [@@deriving sexp, compare, equal]

  include BasicOps with type t := t
  include Comparable.S with type t := t

  val ( ** ) : t -> t -> t
  val one : t
  val of_int : int -> t
  val infinity : t
  val nan : t
  val neg_infinity : t
  val abs : t -> t
  val sign_exn : t -> Sign.t
  val sqrt : t -> t
  val pi : t

  val to_string_hum
    :  ?delimiter:char
    -> ?decimals:int
    -> ?strip_zero:bool
    -> ?explicit_plus:bool
    -> t
    -> string
end

module type Key = sig
  type t [@@deriving sexp, equal]
end

module type Scope = sig
  type t [@@deriving sexp, equal]
end