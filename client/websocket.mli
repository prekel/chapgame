type t [@@deriving sexp, equal]

val connect : Uri.t -> t
val send : t -> msg:string -> unit
val close : t -> unit
val stream : t -> string Lwt_stream.t
