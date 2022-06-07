open Core

module type S = sig
  module N : sig
    type t
  end

  type t [@@deriving sexp, equal]

  val of_list : (int * N.t) list -> eps:N.t -> t
  val derivative : t -> t
  val calc : t -> x:N.t -> N.t
  val degree : t -> int
  val to_map : t -> (int, N.t, Int.comparator_witness) Map.t
  val of_map : (int, N.t, Int.comparator_witness) Map.t -> eps:N.t -> t
  val to_string_hum : ?var:string -> t -> string
end

module type Intf = sig
  module type S = S

  module Make (N : Common.Module_types.NUMBER) : S with module N = N
end
