module Make (N : Module_types.Number) (C : Module_types.Constants with module N = N) =
struct
  module SC = Scene.Make (N) (C)

  module Request = struct
    type t = Action of SC.Action.t [@@deriving sexp, equal]
  end

  module Response = struct
    type t = Diff of SC.Model.Diff.diff [@@deriving sexp, equal]
  end
end
