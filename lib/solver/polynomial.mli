open Core

module Make (N :Module_types.NUMBER) : sig
  type t [@@deriving sexp, equal]

  val of_list : (int * N.t) list -> eps:N.t -> t
  val derivative : t -> t
  val calc : t -> x:N.t -> N.t
  val degree : t -> int
  val to_map : t -> (int, N.t, Int.comparator_witness) Map.t
  val of_map : (int, N.t, Int.comparator_witness) Map.t -> eps:N.t -> t
  val to_string_hum : ?var:string -> t -> string
end
