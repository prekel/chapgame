module Make (C : Module_types.CONSTS) = struct
  module S = Scene.Make (C)

  module Request = struct
    type t =
      | Action of S.Action.t
      | Start of S.Model.t
    [@@deriving sexp, equal]
  end

  module Response = struct
    type t = Diff of S.Model.Diff.t [@@deriving sexp, equal]
  end
end
