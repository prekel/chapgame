open Core

module MakeIntId (Name : sig
  val module_name : string
end) : sig
  include Identifiable.S

  val next : unit -> t
end = struct
  module T = struct
    type t = int [@@deriving bin_io, hash, compare, sexp]

    include Sexpable.To_stringable (struct
      type nonrec t = t [@@deriving sexp]
    end)

    let module_name = Name.module_name
  end

  include T
  include Identifiable.Make (T)

  let current = ref 0

  let next () =
    let ret = !current in
    current := ret + 1;
    ret
  ;;
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
end = struct
  type t = (In.t, In.comparator_witness) Set.t

  let equal = Set.equal
  let t_of_sexp = Set.m__t_of_sexp (module In)
  let sexp_of_t = Set.sexp_of_m__t (module In)
  let to_set = Fn.id
  let to_list = Set.to_list
  let to_sequence a = Set.to_sequence a
  let of_set = Fn.id
  let of_list = Set.of_list (module In)
  let of_sequence = Set.of_sequence (module In)
  let empty = Set.empty (module In)
  let add a ~el = Set.add a el

  module Diff = struct
    type tt = t
    type value = In.t

    type t =
      { added : In.t list
      ; removed : In.t list
      }
    [@@deriving sexp, equal]

    let empty = { added = []; removed = [] }

    let diff ~old curr =
      Set.symmetric_diff old curr
      |> Sequence.fold ~init:empty ~f:(fun acc -> function
           | First l -> { acc with removed = l :: acc.removed }
           | Second r -> { acc with added = r :: acc.added })
    ;;

    let apply_diff ~diff set =
      let removed = List.fold diff.removed ~init:set ~f:Set.remove in
      let added = List.fold diff.added ~init:removed ~f:Set.add in
      added
    ;;
  end
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

  module Diff :
    AdvancedMapDiff with type tt = t and type key = Key.t and type value = Value.t
end = struct
  type t = (Key.t, Value.t, Key.comparator_witness) Map.t

  let equal = Map.equal Value.equal
  let sexp_of_t = Map.sexp_of_m__t (module Key) Value.sexp_of_t
  let t_of_sexp = Map.m__t_of_sexp (module Key) Value.t_of_sexp
  let empty = Map.empty (module Key)
  let add t ~id ~body = Map.add_exn t ~key:id ~data:body
  let to_sequence t = Map.to_sequence t
  let get_by_id t ~id = Map.find_exn t id
  let update_by_id t ~id ~body = Map.update t id ~f:(fun _ -> body)
  let to_map = Fn.id
  let of_map = Fn.id
  let of_alist_exn = Map.of_alist_exn (module Key)
  let find_exn = Map.find_exn

  module Diff = struct
    type key = Key.t
    type value = Value.t
    type tt = t

    type t =
      { added : (Key.t * Value.t) list
      ; changed : (Key.t * Value.t) list
      ; removed : Key.t list
      }
    [@@deriving sexp, equal]

    let empty = { added = []; changed = []; removed = [] }

    let diff ~old curr =
      Map.fold_symmetric_diff
        old
        curr
        ~data_equal:Value.equal
        ~init:empty
        ~f:(fun acc (key, v) ->
          match v with
          | `Left _ -> { acc with removed = key :: acc.removed }
          | `Right r -> { acc with added = (key, r) :: acc.added }
          | `Unequal (_, r) -> { acc with changed = (key, r) :: acc.changed })
    ;;

    let apply_diff ~diff map =
      let removed = List.fold diff.removed ~init:map ~f:Map.remove in
      let added =
        List.fold diff.added ~init:removed ~f:(fun acc (key, data) ->
            Map.add_exn acc ~key ~data)
      in
      let changed =
        List.fold diff.changed ~init:added ~f:(fun acc (key, data) ->
            Map.update acc key ~f:(fun _ -> data))
      in
      changed
    ;;
  end
end
