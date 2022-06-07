open Core
open Open

type t =
  { interval :
      [ `Interval of ExprCoef.t_scalar * ExprCoef.t_scalar
      | `PosInfinity of ExprCoef.t_scalar
      ]
  ; x : Formula.t
  ; y : Formula.t
  ; v_x : Formula.t
  ; v_y : Formula.t
  ; after : t list
  ; name : string
  }
[@@deriving equal]

module Exprs = struct
  open ExprCoef.Syntax

  let g = scope ~scope:`Global (scalar_var `g)
  let mu = scalar_var `mu
  let v0_vec = vector_var `v0_x `v0_y

  let a_vec =
    let f = mu * g in
    -vector_unit v0_vec * vector_of_scalar f f
  ;;

  let a_x = vector_x a_vec
  let a_y = vector_y a_vec
  let v0_x = vector_x v0_vec
  let v0_y = vector_y v0_vec
  let x0 = scalar_var `x0
  let y0 = scalar_var `y0
  let r = scalar_var `r
  let zero = scalar_const N.zero
  let two = scalar_const N.(one + one)
end

let rec rules1 =
  ExprCoef.Syntax.
    [ { interval = `Interval Exprs.(zero, vector_length v0_vec / vector_length a_vec)
      ; x = Formula.of_alist_exn Exprs.[ 0, x0; 1, v0_x; 2, a_x / two ]
      ; y = Formula.of_alist_exn Exprs.[ 0, y0; 1, v0_y; 2, a_y / two ]
      ; v_x = Formula.of_alist_exn Exprs.[ 0, v0_x; 1, a_x ]
      ; v_y = Formula.of_alist_exn Exprs.[ 0, v0_y; 1, a_y ]
      ; after = rules1
      ; name = "rules1_0"
      }
    ; { interval = `PosInfinity Exprs.(vector_length v0_vec / vector_length a_vec)
      ; x =
          Formula.of_alist_exn
            Exprs.
              [ ( 0
                , x0
                  + (v0_x * vector_length v0_vec / vector_length a_vec)
                  + (a_x * sqr (vector_length v0_vec) / (two * sqr (vector_length a_vec)))
                )
              ]
      ; y =
          Formula.of_alist_exn
            Exprs.
              [ ( 0
                , y0
                  + (v0_y * vector_length v0_vec / vector_length a_vec)
                  + (a_y * sqr (vector_length v0_vec) / (two * sqr (vector_length a_vec)))
                )
              ]
      ; v_x = Formula.of_alist_exn Exprs.[ 0, zero ]
      ; v_y = Formula.of_alist_exn Exprs.[ 0, zero ]
      ; after = rules0
      ; name = "rules1_1"
      }
    ]

and rules0 =
  [ { interval = `PosInfinity Exprs.zero
    ; x = Formula.of_alist_exn Exprs.[ 0, x0 ]
    ; y = Formula.of_alist_exn Exprs.[ 0, y0 ]
    ; v_x = Formula.of_alist_exn Exprs.[ 0, zero ]
    ; v_y = Formula.of_alist_exn Exprs.[ 0, zero ]
    ; after = rules0
    ; name = "rules0_0"
    }
  ]
;;

let rules1_0, rules1_1, rules0_0 =
  match rules1, rules0 with
  | [ rules1_0; rules1_1 ], [ rules0_0 ] -> rules1_0, rules1_1, rules0_0
  | _ -> assert false
;;

let sexp_of_t { name; _ } = Sexp.Atom name

let t_of_sexp = function
  | Sexp.Atom "rules1_0" -> rules1_0
  | Sexp.Atom "rules1_1" -> rules1_1
  | Sexp.Atom "rules0_0" -> rules0_0
  | _ -> assert false
;;

let of_values values =
  let open N in
  if Values.get_scalar_exn values ~var:`v0_x = zero
     && Values.get_scalar_exn values ~var:`v0_y = zero
  then rules0
  else rules1
;;
