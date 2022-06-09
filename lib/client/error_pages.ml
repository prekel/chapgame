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
  div
    ~attr:(classes [ "box" ])
    [ p
        [ text "Not available in Github Pages version. You can try "
        ; a ~attr:(many [ href "http://158.101.208.47:25565/" ]) [ text "this" ]
        ; text " (if not work, working link may be in "
        ; a
            ~attr:
              (many [ href "https://github.com/prekel/chapgame/blob/master/README.md" ])
            [ text "README.md" ]
        ; text "."
        ]
    ]
;;
