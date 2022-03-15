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

  module MetrePerSec = struct
    type t
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
    ; mass : Kg.t
    ; velocity : MetrePerSec.t Vec.t option
    ; force : Force.t
    ; r : Metre.t
    ; x : Metre.t
    ; y : Metre.t
    }

  let collision : t -> t -> float = assert false
end

module Scene = struct
  type t =
    { time : float
    ; figures : (Figure.Id.t, Figure.t, unit) Map.t
    }
end

module Events = struct
  type t =
    | Init of (Figure.Id.t, Figure.t, unit) Map.t
    | BodiesMoved 
end

module Actions = struct
  type t = GiveVelocity of Figure.Id.t * Figure.MetrePerSec.t
end
