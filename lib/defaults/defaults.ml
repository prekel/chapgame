open Core
module C = (val Engine.Utils.make_consts ~eps:1e-6)
module S = Engine.Scene.Make (C)
module Request = Protocol.Request.Make (C) (S)

module Response =
  Protocol.Response.Make (C) (S)
    (struct
      include Unit
    end)
