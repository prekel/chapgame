type t

val connect : Uri.t -> var:string Bonsai.Var.t -> t
val send : t -> string -> unit
