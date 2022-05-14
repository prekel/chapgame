module Make
    (C : Engine.Module_types.CONSTS)
    (S : module type of Engine.Scene.Make (C)) =
    struct
  module N = C.N

  type t =
    { time : N.t
    ; speed : N.t
    ; action : [ `Action of S.Action.t | `Replace of S.Model.t ]
    }
  [@@deriving sexp, equal]
end
