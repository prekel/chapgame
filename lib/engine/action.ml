open Core
open Open

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
