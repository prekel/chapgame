module Make
    (N : Module_types.NUMBER)
    (Polynomial : module type of Polynomial.Make (N)) : sig
  val root : a:N.t -> b:N.t -> N.t
  val root_opt : a:N.t -> b:N.t -> N.t option
  val root_poly : Polynomial.t -> N.t option
end
