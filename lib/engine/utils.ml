open Core

module type FLOAT_CONSTS = Module_types.CONSTS with module N = Float

let make_consts ~eps =
  (module struct
    module N = Float

    let eps = eps
  end : FLOAT_CONSTS)
;;
