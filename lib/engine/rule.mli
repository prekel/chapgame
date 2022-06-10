type t =
  { interval :
      [ `Interval of Open.ExprCoef.t_scalar * Open.ExprCoef.t_scalar
      | `PosInfinity of Open.ExprCoef.t_scalar
      ]
  ; x : Open.Formula.t
  ; y : Open.Formula.t
  ; v_x : Open.Formula.t
  ; v_y : Open.Formula.t
  ; after : t list
  ; name : string
  }
[@@deriving equal, sexp]

module Exprs : sig
  val g : float Open.ExprCoef.t
  val mu : float Open.ExprCoef.t
  val v0_vec : (float * float) Open.ExprCoef.t
  val a_vec : (float * float) Open.ExprCoef.t
  val a_x : float Open.ExprCoef.t
  val a_y : float Open.ExprCoef.t
  val v0_x : float Open.ExprCoef.t
  val v0_y : float Open.ExprCoef.t
  val x0 : float Open.ExprCoef.t
  val y0 : float Open.ExprCoef.t
  val r : float Open.ExprCoef.t
end

val rules1_0 : t
val rules1_1 : t
val rules0_0 : t
val rules1 : t list
val rules0 : t list
val of_values : Values.t -> t list

val calc
  :  values:(Open.Var.t -> float)
  -> rules:t list
  -> scoped_values:(Open.Scope.t -> Open.Var.t -> float)
  -> t:float
  -> ((float * float * float * float) * t list) option
