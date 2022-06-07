open Core
open Open

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
