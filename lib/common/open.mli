module Core : sig
  include module type of Core

  module List : sig
    include module type of List

    val windowed2 : 'a list -> ('a * 'a) list
  end
end

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val ( << ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
