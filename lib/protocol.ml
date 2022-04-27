module type S = sig
  module N : Module_types.NUMBER
  module C : Module_types.CONSTS with module N = N
  module S : module type of Scene.Make (N) (C)

  module Request : sig
    type t =
      | Action of S.Action.t
      | Start of S.Model.t
    [@@deriving sexp, equal]
  end

  module Response : sig
    type t = Diff of S.Model.Diff.t [@@deriving sexp, equal]
  end
end

module Make (N : Module_types.NUMBER) (C : Module_types.CONSTS with module N = N) :
  S with module N = N and module C = C = struct
  module N = N
  module C = C
  module S = Scene.Make (N) (C)

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
