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
