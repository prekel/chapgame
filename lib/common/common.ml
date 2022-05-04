module Core = struct
  include Core

  module List = struct
    include List

    let rec windowed2 = function
      | [] | [ _ ] -> []
      | a :: (b :: _ as d) -> (a, b) :: windowed2 d
    ;;
  end
end

let ( >> ) f g x = g (f x)
let ( << ) f g x = f (g x)

module Module_types = Module_types
