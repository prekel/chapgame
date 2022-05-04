include Common.Module_types

module type KEY = sig
  type t [@@deriving sexp, equal]
end

module type SCOPE = sig
  type t [@@deriving sexp, equal]
end

module type CONSTS = sig
  module N : NUMBER

  val eps : N.t
end
