module Make (N : Module_types.NUMBER) (C : Module_types.CONSTS with module N = N) =
struct
  module S = Scene.Make (N) (C)

  module Request = struct
    type t =
      | Action of S.Action.t
      | Start of S.Model.t
    [@@deriving sexp, equal]
  end

  module Response = struct
    type t = Diff of S.Model.Diff.diff [@@deriving sexp, equal]
  end
end
