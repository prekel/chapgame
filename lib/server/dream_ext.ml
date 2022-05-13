open Core

let sexp ?status ?code ?headers ?mach sexp =
  let body =
    match mach with
    | Some true -> Sexp.to_string_mach sexp
    | _ -> Sexp.to_string_hum sexp
  in
  let response = Dream.response ?status ?code ?headers body in
  Dream.set_header response "Content-Type" "text/plain; charset=utf-8";
  Lwt.return response
;;
