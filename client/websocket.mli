type t [@@deriving sexp, equal]

val connect : Uri.t -> on_message:(string -> unit) -> t
val send : t -> string -> unit
val close : t -> unit
