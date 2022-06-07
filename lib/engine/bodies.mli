open Core

type t [@@deriving sexp, equal]

val calc : t -> t:float -> global_values:Values.t -> t
val add : t -> id:Body.Id.t -> body:Body.t -> t
val empty : t
val to_sequence : t -> (Body.Id.t * Body.t) Sequence.t
val get_by_id : t -> id:Body.Id.t -> Body.t
val update_by_id : t -> id:Body.Id.t -> body:Body.t -> t
val remove : t -> Body.Id.t -> t

module Diff :
  Common.Utils.AdvancedMapDiff
    with type tt = t
     and type key = Body.Id.t
     and type value = Body.t
