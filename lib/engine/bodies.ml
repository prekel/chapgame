open Core
include Common.Utils.MakeAdvancedMap (Body.Id) (Body)

let calc bodies ~t ~global_values =
  bodies
  |> Map.map ~f:(fun body ->
         Rule.calc
           ~values:(Values.to_function body.Body.values)
           ~rules:body.rules
           ~scoped_values:(Values.global_to_scoped global_values)
           ~t
         |> Option.map ~f:(fun (xy, rules) -> Body.update_x0y0 ~body xy ~rules)
         |> Option.value ~default:body)
;;
