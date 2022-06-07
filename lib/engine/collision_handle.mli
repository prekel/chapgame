val collision
  :  v1:float * float
  -> v2:float * float
  -> x1:float * float
  -> x2:float * float
  -> m1:float
  -> m2:float
  -> float * float

val collision_body
  :  v1:float * float
  -> v2:float * float
  -> m1:float
  -> m2:float
  -> x1:float
  -> y1:float
  -> x2:float
  -> y2:float
  -> (float * float) * (float * float)

val calculate_new_v : Values.t -> Values.t -> (float * float) * (float * float)
val calculate_new_v_with_point : body:Body.t -> point:Point.t -> float * float
val calculate_new_v_with_line : body:Body.t -> line:Line.t -> float * float
