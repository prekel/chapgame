open Core

open Open 

module WithBody : sig
  val first_collision
    :  (Body.Id.t * Body.t) Sequence.t
    -> r:Formula.t
    -> global:Values.t
    -> (float * Body.Id.t * Body.Id.t) option
end

module WithPoint : sig
  val first_collision
    :  global:Values.t
    -> (Body.Id.t * Body.t) Sequence.t
    -> points:Point.t Sequence.t
    -> r:Formula.t
    -> (float * Body.Id.t * Point.t) option
end

module WithLine : sig
  val first_collision
    :  global:Values.t
    -> (Body.Id.t * Body.t) Sequence.t
    -> lines:Line.t Sequence.t
    -> r:Formula.t
    -> (float * Body.Id.t * Line.t) option
end
