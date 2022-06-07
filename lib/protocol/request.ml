module Make (C : Engine.Module_types.CONSTS) (S : Engine.Scene.S with module N = C.N) =
struct
  module N = C.N

  type t =
    { time : N.t
    ; speed : N.t
    ; action : [ `Action of S.Action.t | `Replace of S.Model.t ]
    }
  [@@deriving sexp, equal]
end
