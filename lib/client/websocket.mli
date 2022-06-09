type t [@@deriving sexp, equal]

val connect : Uri.t -> t
val send : t -> msg:string -> unit
val close : t -> unit
val stream : t -> string Lwt_stream.t

val use
  :  Uri.t Bonsai.Value.t
  -> ((string -> unit Ui_effect.t) * string option) Bonsai.Computation.t
