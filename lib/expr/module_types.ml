open Core

module type VAR = sig
  include Comparable.S
  include Sexpable.S with type t := t
end

module type SCOPE = sig
  include Equal.S
  include Sexpable.S with type t := t

  val is_global : t -> bool
end
