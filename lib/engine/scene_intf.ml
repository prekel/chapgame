open Core

module type S = sig
  module N : Solver.Module_types.NUMBER

  module Vars : sig
    type t

    val x0 : t
    val y0 : t
    val v0_x : t
    val v0_y : t
    val r : t
    val g : t
    val mu : t
    val m : t
  end

  module Scope : sig
    type t
  end

  module Expr : sig
    type _ t

    val calc
      :  values:(Vars.t -> N.t)
      -> scoped_values:(Scope.t -> Vars.t -> N.t)
      -> (module Common.Module_types.BASIC_OPS with type t = 'result)
      -> 'result t
      -> 'result
  end

  module Point : Point.S with module N = N
  module Line : Line.S with module N = N and module Point = Point

  module Points : sig
    type t

    val to_sequence : t -> Point.t Sequence.t

    module Diff : Common.Utils.AdvancedSetDiff with type tt = t and type value = Point.t
  end

  module Lines : sig
    type t

    val to_sequence : t -> Line.t Sequence.t

    module Diff : Common.Utils.AdvancedSetDiff with type tt = t and type value = Line.t
  end

  module Values : sig
    type t

    val get_scalar_exn : t -> var:Vars.t -> N.t
    val to_function : t -> Vars.t -> N.t
    val global_to_scoped : t -> Scope.t -> Vars.t -> N.t

    module Diff :
      Common.Utils.AdvancedMapDiff
        with type tt = t
         and type key = Vars.t
         and type value = N.t
  end

  module Body : sig
    module Id : Common.Module_types.IDENTIFIABLE

    type t [@@deriving sexp, equal]

    val get_id : t -> Id.t
    val get_values : t -> Values.t
  end

  module Bodies : sig
    type t

    val calc : t -> t:N.t -> global_values:Values.t -> t
    val to_sequence : t -> (Body.Id.t * Body.t) Sequence.t

    module Diff :
      Common.Utils.AdvancedMapDiff
        with type tt = t
         and type key = Body.Id.t
         and type value = Body.t
  end

  module Action : sig
    type a =
      | AddBody of
          { id : Body.Id.t option
          ; x0 : N.t
          ; y0 : N.t
          ; r : N.t
          ; mu : N.t
          ; m : N.t
          }
      | AddBodyOfValues of (Body.Id.t option * (Vars.t * N.t) list)
      | AddPoint of Point.t
      | AddLine of Line.t
      | AddLineWithPoints of Line.t
      | GiveVelocity of
          { id : Body.Id.t
          ; v0 : N.t * N.t
          }
      | RemoveBody of Body.Id.t
      | RemoveLine of Line.t
      | RemovePoint of Point.t
      | UpdateBody of (Body.Id.t * (Vars.t * N.t) list)
      | UpdateLine of Line.t * Line.t
      | UpdatePoint of Point.t * Point.t
      | UpdateGlobal of (Vars.t * N.t)
    [@@deriving sexp, equal, compare]

    type until =
      { timespan : N.t option
      ; quantity : int option
      }
    [@@deriving sexp, equal, compare]

    type t =
      { time : N.t
      ; action : a
      ; until : until
      }
    [@@deriving sexp, equal, compare]
  end

  module Scene : sig
    module Collision : sig
      type t =
        | Collision of
            { id1 : Body.Id.t
            ; id2 : Body.Id.t
            }
        | CollisionWithPoint of
            { id : Body.Id.t
            ; point : Point.t
            }
        | CollisionWithLine of
            { id : Body.Id.t
            ; line : Line.t
            }
      [@@deriving sexp, equal, compare]
    end

    module Cause : sig
      type t =
        [ `Init
        | `Action of Action.t
        | `Collision of Collision.t
        ]
      [@@deriving sexp, equal, compare]
    end

    type t =
      { time : N.t
      ; bodies : Bodies.t
      ; points : Points.t
      ; lines : Lines.t
      ; global_values : Values.t
      ; cause : Cause.t list
      }
    [@@deriving sexp, equal]

    module Diff : sig
      type t =
        { new_time : N.t
        ; bodies_diff : Bodies.Diff.t
        ; points_diff : Points.Diff.t
        ; lines_diff : Lines.Diff.t
        ; global_values_diff : Values.Diff.t
        ; new_cause : Cause.t list
        }
      [@@deriving sexp, equal]
    end
  end

  module Scenes : sig
    type t

    val last_exn : t -> Scene.t
    val to_map : t -> (N.t, Scene.t, N.comparator_witness) Map.t
    val to_sequence : t -> (N.t * Scene.t) Sequence.t
  end

  module Model : sig
    type t =
      { timeout : N.t option
      ; scenes : Scenes.t
      }
    [@@deriving sexp, equal]

    val init : g:N.t -> t

    module Diff : sig
      type tt = t

      type t =
        { init : [ `Init of Scene.t | `Since of N.t ]
        ; scene_diffs : Scene.Diff.t list
        ; new_timeout : N.t option
        }

      include Common.Utils.Diff with type tt := tt and type t := t
    end
  end

  module Engine : sig
    val recv : Model.t -> action:Action.t -> Model.t
    val prolong : Model.t -> until:Action.until -> Model.t
  end
end

module type Intf = sig
  module type S = S

  module Make
      (N : Solver.Module_types.NUMBER) (C : sig
        val eps : N.t
      end) (MakeDeps : functor (Var : Module_types.VAR) (Scope : Module_types.SCOPE) ->
          module type of Deps.Make (N) (Var) (Scope)) : S with module N = N
end
