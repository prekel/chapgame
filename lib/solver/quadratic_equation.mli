module Make
    (N : Module_types.NUMBER)
    (Polynomial : module type of Polynomial.Make (N)) : sig
  val roots : a:N.t -> b:N.t -> c:N.t -> eps:N.t -> N.t list
  val roots_poly : Polynomial.t -> eps:N.t -> N.t list
end
