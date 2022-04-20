open! Core
open! Async_kernel
open! Bonsai_web

module Lwt_fun_arg = struct
  module Action = struct
    type 'r t = T : 'a * ('a -> 'r Lwt.t) -> 'r t
  end

  let handle (Action.T (a, f)) ~on_response =
    Lwt.async (fun () ->
        let%map.Lwt result = f a in
        on_response result)
  ;;
end

module Lwt_fun = Ui_effect.Define1 (Lwt_fun_arg)

let of_lwt_fun f a = Lwt_fun.inject (T (a, f))
