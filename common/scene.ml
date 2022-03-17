open Core

module Figure = struct
  module Vec = struct
    type _ t
  end

  module Kg = struct
    type t
  end

  module Metre = struct
    type t
  end

  module Velocity = struct
    type t = M of float * float
  end

  module Newton = struct
    type t
  end

  module Force = struct
    type t =
      | Const of Newton.t Vec.t
      | Friction of float
      | No
  end

  module Id = struct
    type t
  end

  type t =
    { id : Id.t
    ; m : Kg.t
    ; v0 : Velocity.t option
    ; f : Force.t
    ; r : Metre.t
    ; x : Metre.t
    ; y : Metre.t
    }

  let collision : t -> t -> float = assert false
end

module Figure2 = struct
  module Formula = struct
    module CoefficientVar = struct
      type t = A
    end

    type t = (int, float * CoefficientVar.t, Int.comparator_witness) Map.t
  end

  type t =
    { id : int
    ; x : Formula.t
    }
end

module Scene = struct
  type t = { figures : (Figure.Id.t, Figure.t, unit) Map.t }
end

module Events = struct
  type t =
    | Init of (Figure.Id.t, Figure.t, unit) Map.t
    | BodiesMoved
end

module Action = struct
  type t = GiveVelocity of Time_ns.Span.t * Figure.Id.t * Figure.Velocity.t Figure.Vec.t
end

module Model = struct
  type t = (Time_ns.Span.t, Scene.t, Time_ns.Span.comparator_witness) Map.t

  let e () : t = Map.empty (module Time_ns.Span)
end

module Engine = struct
  let recv model action = model, assert false
end
