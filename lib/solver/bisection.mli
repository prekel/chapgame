module Make (N : Module_types.NUMBER) (Interval : module type of Interval.Make (N)) : sig
  val search : f:(N.t -> N.t) -> eps:N.t -> Interval.t -> N.t option
end
