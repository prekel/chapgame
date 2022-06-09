open Bonsai_web

let tbd =
  let open Vdom in
  let open Node in
  let open Attr in
  div ~attr:(classes [ "box" ]) [ p [ text "To Be Developed" ] ]
;;

let not_found =
  let open Vdom in
  let open Node in
  let open Attr in
  div ~attr:(classes [ "box" ]) [ p [ text "Not found" ] ]
;;

let online_dummy =
  let open Vdom in
  let open Node in
  let open Attr in
  div ~attr:(classes [ "box" ]) [ p [ text "Not available in Github Pages version." ] ];;
