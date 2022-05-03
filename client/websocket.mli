type t [@@deriving sexp, equal]

val connect : Uri.t -> on_message:(string -> unit) -> on_close:(int -> unit) -> t
val send : t -> string -> unit
val close : t -> unit
val stream : Uri.t -> string Lwt_stream.t * t
