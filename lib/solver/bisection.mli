module Make (N : Module_types.NUMBER) (Interval : Interval.S with module N := N) : sig
  (** [search ~f ~eps interval] is [x] where [abs (f x) < eps]. Requires that [f] is
      continuous and non-decreasing (or non-increasing) on [interval]. *)
  val search : f:(N.t -> N.t) -> eps:N.t -> Interval.t -> N.t option
end
