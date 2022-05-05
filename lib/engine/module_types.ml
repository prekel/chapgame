include Common.Module_types

module type KEY = sig
  type t [@@deriving sexp, equal]
end

module type SCOPE = sig
  type t [@@deriving sexp, equal]
end

module type CONSTS = sig
  module N : Solver.Module_types.NUMBER

  val eps : N.t
end
