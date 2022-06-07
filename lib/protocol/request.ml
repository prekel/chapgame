module Make
    (C : Engine.Module_types.CONSTS)
      (MakeDeps : functor
        (Var : Engine.Module_types.VAR)
        (Scope : Engine.Module_types.SCOPE)
        ->
        module type of Engine.Deps.Make (C.N) (Var) (Scope))
    (S : module type of Engine.Scene.Make (C.N) (C) (MakeDeps)) =
    struct
  module N = C.N

  type t =
    { time : N.t
    ; speed : N.t
    ; action : [ `Action of S.Action.t | `Replace of S.Model.t ]
    }
  [@@deriving sexp, equal]
end
