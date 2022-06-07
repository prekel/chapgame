module type S = sig
  module N : sig
    type t
  end

  include Module_types.BASIC_OPS with type t = N.t * N.t

  (** [dot v1 v2] is dot-product (scalar product) of vector [v1] and [v2] *)
  val dot : t -> t -> N.t

  (** [let_sqr v] is squared length of vector [v] *)
  val len_sqr : t -> N.t

  (** [let_len v] is length of vector [v] *)
  val len : t -> N.t

  (** [v *^ s] is [v] scaled by [s] *)
  val ( *^ ) : t -> N.t -> t

  (** [s ^* v] is [v] scaled by [s] *)
  val ( ^* ) : N.t -> t -> t

  val ( = ) : t -> t -> bool
  val unit : t -> t
end

module type Intf = sig
  module type S = S

  module Make (N : Module_types.NUMBER) : S with module N = N
end
