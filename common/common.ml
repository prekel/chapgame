module List = struct
  let rec windowed2_exn list =
    match list with
    | [] | [ _ ] -> []
    | a :: (b :: _ as d) -> (a, b) :: windowed2_exn d
  ;;
end
