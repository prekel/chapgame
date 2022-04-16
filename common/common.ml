include Core

module List = struct
  include List

  let rec windowed2 = function
    | [] | [ _ ] -> []
    | a :: (b :: _ as d) -> (a, b) :: windowed2 d
  ;;
end

module Fn = struct
  include Fn

  let ( >> ) f g x = g (f x)
  let ( << ) f g x = f (g x)
end
