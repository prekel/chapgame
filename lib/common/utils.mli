open Core

module MakeIntId (Name : sig
  val module_name : string
end) : sig
  include Identifiable.S

  val next : unit -> t
end

module type Diff = sig
  type tt
  type t [@@deriving sexp, equal]

  val diff : old:tt -> tt -> t
  val apply_diff : diff:t -> tt -> tt
end

module type AdvancedSetDiff = sig
  type value
  type tt

  type t =
    { added : value list
    ; removed : value list
    }
  [@@deriving sexp, equal]

  include Diff with type tt := tt and type t := t
end

module MakeAdvancedSet (In : sig
  include Comparable.S
  include Sexpable.S with type t := t
end) : sig
  type t = (In.t, In.comparator_witness) Set.t [@@deriving sexp, equal]

  val to_set : t -> (In.t, In.comparator_witness) Set.t
  val to_list : t -> In.t list
  val to_sequence : t -> In.t Sequence.t
  val of_set : (In.t, In.comparator_witness) Set.t -> t
  val of_list : In.t list -> t
  val of_sequence : In.t Sequence.t -> t
  val empty : t
  val add : t -> el:In.t -> t

  module Diff : AdvancedSetDiff with type tt = t and type value = In.t
end

module type AdvancedMapDiff = sig
  type key
  type value
  type tt

  type t =
    { added : (key * value) list
    ; changed : (key * value) list
    ; removed : key list
    }
  [@@deriving sexp, equal]

  include Diff with type tt := tt and type t := t
end

module MakeAdvancedMap (Key : sig
  include Comparable.S
  include Sexpable.S with type t := t
end) (Value : sig
  type t [@@deriving sexp, equal]
end) : sig
  type t = (Key.t, Value.t, Key.comparator_witness) Map.t [@@deriving sexp, equal]

  val add : t -> id:Key.t -> body:Value.t -> t
  val empty : t
  val to_sequence : t -> (Key.t * Value.t) Sequence.t
  val get_by_id : t -> id:Key.t -> Value.t
  val update_by_id : t -> id:Key.t -> body:Value.t -> t
  val to_map : t -> (Key.t, Value.t, Key.comparator_witness) Map.t
  val of_map : (Key.t, Value.t, Key.comparator_witness) Map.t -> t
  val of_alist_exn : (Key.t * Value.t) list -> t
  val find_exn : t -> Key.t -> Value.t
  val remove : t -> Key.t -> t

  module Diff :
    AdvancedMapDiff with type tt = t and type key = Key.t and type value = Value.t
end
