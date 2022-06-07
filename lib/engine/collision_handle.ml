open Core
open Open

let collision ~v1 ~v2 ~x1 ~x2 ~m1 ~m2 =
  let module V = Vector in
  let open Float in
  let two = one + one in
  let q2 =
    match m1 = infinity, m2 = infinity with
    | true, true -> one
    | true, false -> zero
    | false, true -> two
    | false, false -> two * m2 / (m1 + m2)
  in
  let q3 = V.(dot (v1 - v2) (x1 - x2)) / square V.(len (x2 - x1)) in
  V.(v1 - (N.(q2 * q3) ^* (x1 - x2)))
;;

let collision_body ~v1 ~v2 ~m1 ~m2 ~x1 ~y1 ~x2 ~y2 =
  let v1' = collision ~v1 ~v2 ~x1:(x1, y1) ~x2:(x2, y2) ~m1 ~m2 in
  let v2' = collision ~v1:v2 ~v2:v1 ~x1:(x2, y2) ~x2:(x1, y1) ~m1:m2 ~m2:m1 in
  v1', v2'
;;

let calculate_new_v values1 values2 =
  let v1 = Values.get_vector_exn values1 ~var_x:`v0_x ~var_y:`v0_y in
  let v2 = Values.get_vector_exn values2 ~var_x:`v0_x ~var_y:`v0_y in
  let m1 = Values.get_scalar_exn values1 ~var:`m in
  let m2 = Values.get_scalar_exn values2 ~var:`m in
  let x1 = Values.get_scalar_exn values1 ~var:`x0 in
  let y1 = Values.get_scalar_exn values1 ~var:`y0 in
  let x2 = Values.get_scalar_exn values2 ~var:`x0 in
  let y2 = Values.get_scalar_exn values2 ~var:`y0 in
  let v1', v2' = collision_body ~v1 ~v2 ~m1 ~m2 ~x1 ~y1 ~x2 ~y2 in
  (* TODO: inf m *)
  let v1' = if Float.(m1 = infinity) then v1 else v1' in
  let v2' = if Float.(m2 = infinity) then v2 else v2' in
  v1', v2'
;;

let calculate_new_v_with_point ~body ~point:Point.{ x = x2; y = y2 } =
  let v1 = Values.get_vector_exn body.Body.values ~var_x:`v0_x ~var_y:`v0_y in
  let m1 = Values.get_scalar_exn body.values ~var:`m in
  let x1 = Values.get_scalar_exn body.values ~var:`x0 in
  let y1 = Values.get_scalar_exn body.values ~var:`y0 in
  fst @@ collision_body ~v1 ~v2:Float.(zero, zero) ~m1 ~m2:Float.infinity ~x1 ~y1 ~x2 ~y2
;;

let calculate_new_v_with_line ~body ~line =
  let a, b, _ = Line.to_abc line in
  let v1 = Values.get_vector_exn body.Body.values ~var_x:`v0_x ~var_y:`v0_y in
  let m1 = Values.get_scalar_exn body.values ~var:`m in
  let x1 = Values.get_scalar_exn body.values ~var:`x0 in
  let y1 = Values.get_scalar_exn body.values ~var:`y0 in
  let r = Values.get_scalar_exn body.values ~var:`r in
  let v2 = Float.(zero, zero) in
  let m2 = Float.infinity in
  let module V = Vector in
  let x2, y2 = V.((x1, y1) + (unit (a, b) *^ r)) in
  fst @@ collision_body ~v1 ~v2 ~m1 ~m2 ~x1 ~y1 ~x2 ~y2
;;
