module Make (N : Module_types.NUMBER) : sig
  module P : module type of Polynomial.Make (N)

  val roots : eps:N.t -> P.t -> N.t list
end
