module Make (N : Solver.Module_types.NUMBER) : sig
  include Common.Module_types.BASIC_OPS with type t = N.t * N.t

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
end
