module type S = sig
  module N : Module_types.NUMBER
  module Interval : Interval.S with module N = N

  (** [search ~f ~eps interval] is [x] where [abs (f x) < eps]. Requires that [f] is
      continuous and non-decreasing (or non-increasing) on [interval]. *)
  val search : f:(N.t -> N.t) -> eps:N.t -> Interval.t -> N.t option
end

module type Intf = sig
  module type S = S

  module Make (N : Module_types.NUMBER) (Interval : Interval.S with module N = N) :
    S with module N = N and module Interval = Interval
end
