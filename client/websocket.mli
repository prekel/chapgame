type t

val connect : Uri.t -> var:string list Bonsai.Var.t -> t
val send : t -> string -> unit
