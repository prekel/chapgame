open Core

module Make (C : Module_types.CONSTS) (S : module type of Scene.Make (C)) = struct
  module Request = struct
    type t =
      | Action of S.Action.t
      | Start of S.Model.t
      | Replace of S.Model.t
    [@@deriving sexp, equal]
  end

  module Response = struct
    type t =
      | Diff of S.Model.Diff.t
      | Replace of S.Model.t
    [@@deriving sexp, equal]
  end
end

module Make1
    (C : Module_types.CONSTS)
    (S : module type of Scene.Make (C)) (Payload : sig
      include Sexpable.S
      include Equal.S with type t := t
    end) =
    struct
  module N = C.N

  module Request = struct
    type t =
      { time : N.t
      ; speed : N.t
      ; action : [ `Action of S.Action.t | `Replace of S.Model.t ]
      }
    [@@deriving sexp, equal]
  end

  module Response = struct
    type t =
      { time : N.t
      ; speed : N.t
      ; payload : Payload.t
      ; diff : [ `Diff of S.Model.Diff.t | `Replace of S.Model.t ]
      }
    [@@deriving sexp, equal]
  end
end
