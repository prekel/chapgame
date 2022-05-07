module Make
    (N : Module_types.NUMBER)
    (I : module type of Interval.Make (N))
    (P : module type of Polynomial.Make (N))
    (LE : module type of Linear_equation.Make (N) (P))
    (QE : module type of Quadratic_equation.Make (N) (P))
    (BS : module type of Bisection.Make (N) (I)) : sig
      (** [roots ~eps poly] is roots of equation [poly] = 0 when
          [abs (calc poly ~x) < eps]*)
      val roots : eps:N.t -> P.t -> N.t list
    end
