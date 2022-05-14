module Make
    (C : Engine.Module_types.CONSTS)
    (S : module type of Engine.Scene.Make (C)) (Payload : sig
      type t [@@deriving sexp, equal]
    end) =
    struct
  module N = C.N

  type f =
    { time : N.t
    ; speed : N.t
    ; payload : Payload.t
    ; diff : [ `Diff of S.Model.Diff.t | `Replace of S.Model.t ]
    }
  [@@deriving sexp, equal]

  type t =
    | Full of f
    | Chunk of S.Model.Diff.t
  [@@deriving sexp, equal]
end
