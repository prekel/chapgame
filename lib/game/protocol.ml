module Make
    (C : Engine.Module_types.CONSTS)
    (S : module type of Engine.Scene.Make (C))
    (P : module type of Engine.Protocol.Make (C) (S)) =
    struct
      module Request = struct
        type t =
          | GiveVelocity of
              { id : S.Body.Id.t
              ; v : C.N.t * C.N.t
              }
      end

      module Response = struct
        type t =
          | Engine of P.Response.t
          | Game of [ `End ]
      end
    end
