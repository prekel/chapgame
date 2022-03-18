open Core

module type Number = sig
  type t [@@deriving sexp, compare, equal]

  include Comparable.S with type t := t

  val ( ~- ) : t -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( ** ) : t -> t -> t
  val zero : t
  val one : t
  val of_int : int -> t
  val infinity : t
  val nan : t
  val neg_infinity : t
  val abs : t -> t
  val sign_exn : t -> Sign.t
  val sqrt : t -> t 
end
