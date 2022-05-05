module type BASIC_OPS = sig
  type t [@@deriving equal]

  val zero : t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( ~- ) : t -> t
end
