include Core

module List = struct
  include List

  let rec windowed2_exn = function
    | [] | [ _ ] -> []
    | a :: (b :: _ as d) -> (a, b) :: windowed2_exn d
  ;;
end

module Fn = struct
  include Fn

  let ( >> ) f g x = g (f x)
  let ( << ) f g x = f (g x)
end

module Map = struct
  include Map

  let sexp_of_t sexp_of_key sexp_of_value map =
    Map.to_alist map |> List.Assoc.sexp_of_t sexp_of_key sexp_of_value
  ;;

  let t_of_sexp key_of_sexp value_of_sexp m sexp =
    sexp |> List.Assoc.t_of_sexp key_of_sexp value_of_sexp |> Map.of_alist_exn m
  ;;
end
