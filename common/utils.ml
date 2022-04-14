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

module MakeAdvancedSet (In : sig
  include Comparable.S
  include Sexpable.S with type t := t
end) : sig
  type t [@@deriving sexp, equal]

  val to_set : t -> (In.t, In.comparator_witness) Set.t
  val to_list : t -> In.t list
  val to_sequence : t -> In.t Sequence.t
  val of_set : (In.t, In.comparator_witness) Set.t -> t
  val of_list : In.t list -> t
  val of_sequence : In.t Sequence.t -> t
  val empty : t
  val add : t -> el:In.t -> t
end = struct
  type t = (In.t, In.comparator_witness) Set.t

  let equal = Set.equal
  let t_of_sexp a = a |> List.t_of_sexp In.t_of_sexp |> Set.of_list (module In)
  let sexp_of_t a = a |> Set.to_list |> List.sexp_of_t In.sexp_of_t
  let to_set = Fn.id
  let to_list = Set.to_list
  let to_sequence a = Set.to_sequence a
  let of_set = Fn.id
  let of_list = Set.of_list (module In)
  let of_sequence = Set.of_sequence (module In)
  let empty = Set.empty (module In)
  let add a ~el = Set.add a el
end

module MakeAdvancedMap (Key : sig
  include Comparable.S
  include Sexpable.S with type t := t
end) (Value : sig
  type t [@@deriving sexp, equal]
end) : sig
  type t [@@deriving sexp, equal]

  val add : t -> id:Key.t -> body:Value.t -> t
  val empty : t
  val to_sequence : t -> (Key.t * Value.t) Sequence.t
  val get_by_id : t -> id:Key.t -> Value.t
  val update_by_id : t -> id:Key.t -> body:Value.t -> t
  val to_map : t -> (Key.t, Value.t, Key.comparator_witness) Map.t
  val of_map : (Key.t, Value.t, Key.comparator_witness) Map.t -> t
end = struct
  type t = (Key.t, Value.t, Key.comparator_witness) Map.t

  let equal = Map.equal Value.equal
  let sexp_of_t = Common.Map.sexp_of_t Key.sexp_of_t Value.sexp_of_t
  let t_of_sexp = Common.Map.t_of_sexp Key.t_of_sexp Value.t_of_sexp (module Key)
  let empty = Map.empty (module Key)
  let add t ~id ~body = Map.add_exn t ~key:id ~data:body
  let to_sequence t = Map.to_sequence t
  let get_by_id t ~id = Map.find_exn t id
  let update_by_id t ~id ~body = Map.update t id ~f:(fun _ -> body)
  let to_map = Fn.id
  let of_map = Fn.id
end
