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
