open Core

module Var : sig
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

module Point : sig
  type t =
    { x : float
    ; y : float
    }

  include Sexpable.S with type t := t
  include Comparable.S with type t := t
end

module Line : sig
  type kind =
    [ `Line
    | `Segment
    | `Ray
    ]
  [@@deriving sexp, equal, compare]

  type t [@@deriving sexp]

  include Comparable.S with type t := t

  val of_points : p1:Point.t -> p2:Point.t -> kind:[ `Line | `Ray | `Segment ] -> t
  val to_points : t -> Point.t * Point.t
  val kind : t -> kind
end

module Points : sig
  type t

  val to_sequence : t -> Point.t Sequence.t
  val to_list : t -> Point.t list

  module Diff : Common.Utils.AdvancedSetDiff with type tt = t and type value = Point.t
end

module Lines : sig
  type t

  val to_sequence : t -> Line.t Sequence.t
  val to_list : t -> Line.t list

  module Diff : Common.Utils.AdvancedSetDiff with type tt = t and type value = Line.t
end

module Values : sig
  type t

  val get_scalar_exn : t -> var:Var.t -> float
  val to_function : t -> Var.t -> float
  val global_to_scoped : t -> Scope.t -> Var.t -> float

  module Diff :
    Common.Utils.AdvancedMapDiff
      with type tt = t
       and type key = Var.t
       and type value = float
end

module Body : sig
  module Id : Common.Module_types.IDENTIFIABLE

  type t [@@deriving sexp, equal]

  val get_id : t -> Id.t
  val get_values : t -> Values.t
  val calc_a : global_values:Values.t -> t -> float * float
end

module Bodies : sig
  type t

  val calc : t -> t:float -> global_values:Values.t -> t
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
        ; x0 : float
        ; y0 : float
        ; r : float
        ; mu : float
        ; m : float
        }
    | AddBodyOfValues of (Body.Id.t option * (Var.t * float) list)
    | AddPoint of Point.t
    | AddLine of Line.t
    | AddLineWithPoints of Line.t
    | GiveVelocity of
        { id : Body.Id.t
        ; v0 : float * float
        }
    | RemoveBody of Body.Id.t
    | RemoveLine of Line.t
    | RemovePoint of Point.t
    | UpdateBody of (Body.Id.t * (Var.t * float) list)
    | UpdateLine of Line.t * Line.t
    | UpdatePoint of Point.t * Point.t
    | UpdateGlobal of (Var.t * float)
  [@@deriving sexp, equal, compare]

  type until =
    { timespan : float option
    ; quantity : int option
    }
  [@@deriving sexp, equal, compare]

  type t =
    { time : float
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
    { time : float
    ; bodies : Bodies.t
    ; points : Points.t
    ; lines : Lines.t
    ; global_values : Values.t
    ; cause : Cause.t list
    }
  [@@deriving sexp, equal]

  module Diff : sig
    type t =
      { new_time : float
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

  val before : t -> time:float -> t * Scene.t
  val last_exn : t -> Scene.t
  val to_map : t -> (float, Scene.t, Float.comparator_witness) Map.t
  val to_sequence : t -> (float * Scene.t) Sequence.t
end

module Model : sig
  type t =
    { timeout : float option
    ; scenes : Scenes.t
    }
  [@@deriving sexp, equal]

  val init : g:float -> t

  module Diff : sig
    type tt = t

    type t =
      { init : [ `Init of Scene.t | `Since of float ]
      ; scene_diffs : Scene.Diff.t list
      ; new_timeout : float option
      }

    include Common.Utils.Diff with type tt := tt and type t := t
  end
end

val recv : Model.t -> action:Action.t -> Model.t
val prolong : Model.t -> until:Action.until -> Model.t

val update
  :  Model.t
  -> action:
       [< `Action of Action.t
       | `Diff of Model.Diff.t
       | `Prolong of Action.until
       | `Replace of Model.t
       ]
  -> Model.t

val recv_with_diff : Model.t -> action:[< `Action of Action.t ] -> Model.t * Model.Diff.t
