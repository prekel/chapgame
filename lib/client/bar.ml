open Core
open Bonsai_web
open Bonsai.Let_syntax
open Js_of_ocaml

let tab_text = function
  | `Offline -> "Offline"
  | `Online -> "Online"
;;

let component ~inner ~outer ~opened_tab ~tab_click =
  let%sub info_modal_is_open, set_info_modal_is_open =
    Bonsai.state [%here] (module Bool) ~default_model:false
  in
  let%arr inner = inner
  and outer = outer
  and tab_click = tab_click
  and info_modal_is_open = info_modal_is_open
  and set_info_modal_is_open = set_info_modal_is_open in
  let open Vdom in
  let open Node in
  let open Attr in
  div
    ~attr:(many [ classes [ "columns"; "is-gapless" ] ])
    [ div
        ~attr:(many [ classes [ "column"; "is-narrow"; "bar-column" ] ])
        [ Node.div
            ~attr:(many [])
            [ Node.create
                "nav"
                ~attr:(many [ classes [ "navbar" ] ])
                [ div
                    ~attr:(many [ classes [ "navbar-brand" ] ])
                    [ a
                        ~attr:
                          (many
                             [ classes [ "navbar-item" ]
                             ; on_click (fun _ ->
                                   Dom_html.window##.location##reload;
                                   Effect.Ignore)
                             ])
                        [ text "chapgame" ]
                    ]
                ; div
                    ~attr:(many [ classes [ "navbar-menu" ] ])
                    [ div
                        ~attr:(many [ classes [ "navbar-end" ] ])
                        [ a
                            ~attr:
                              (many
                                 [ classes [ "navbar-item" ]
                                 ; on_click (fun _ -> set_info_modal_is_open true)
                                 ])
                            [ span
                                ~attr:(class_ "icon")
                                [ Node.create
                                    "i"
                                    ~attr:(classes [ "fas"; "fa-question-circle" ])
                                    []
                                ]
                            ]
                        ; a
                            ~attr:
                              (many
                                 [ classes [ "navbar-item" ]
                                 ; href "https://github.com/prekel/chapgame"
                                 ])
                            [ span
                                ~attr:(class_ "icon")
                                [ Node.create
                                    "i"
                                    ~attr:(classes [ "fab"; "fa-github" ])
                                    []
                                ]
                            ]
                        ]
                    ]
                ]
            ; div
                ~attr:(many [ classes [ "tabs"; "is-centered" ] ])
                [ ul
                    [ li
                        ~attr:
                          (many
                             [ (match opened_tab with
                               | `Offline -> class_ "is-active"
                               | _ -> empty)
                             ; on_click (fun _ -> tab_click `Offline)
                             ])
                        [ a [ Node.text (tab_text `Offline) ] ]
                    ; li
                        ~attr:
                          (many
                             [ (match opened_tab with
                               | `Online -> class_ "is-active"
                               | _ -> empty)
                             ; on_click (fun _ -> tab_click `Online)
                             ])
                        [ a [ Node.text (tab_text `Online) ] ]
                    ]
                ]
            ; div
                ~attr:(many [ classes [ "scroll-y" ] ])
                [ div ~attr:(many [ classes [ "directionltr" ] ]) [ inner ] ]
            ]
        ]
    ; div ~attr:(many [ classes [ "column" ] ]) [ outer ]
    ; (if info_modal_is_open
      then
        div
          ~attr:(classes [ "modal"; "is-active" ])
          [ div
              ~attr:
                (many
                   [ class_ "modal-background"
                   ; on_click (fun _ -> set_info_modal_is_open false)
                   ])
              []
          ; div
              ~attr:(many [ class_ "modal-card" ])
              [ header
                  ~attr:(many [ class_ "modal-card-head" ])
                  [ p ~attr:(many [ class_ "modal-card-title" ]) [ text "Info" ]
                  ; button
                      ~attr:
                        (many
                           [ class_ "delete"
                           ; on_click (fun _ -> set_info_modal_is_open false)
                           ])
                      []
                  ]
              ; section
                  ~attr:(class_ "modal-card-body")
                  [ text
                      "This model simulates the movement and collisions of bodies moving \
                       uniformly accelerated (uniformly slowed down) under the action of \
                       friction force. An "
                  ; a
                      ~attr:
                        (href
                           "https://en.wikipedia.org/wiki/Collision_detection#Collision_detection_in_computer_simulation")
                      [ text "a priori approach" ]
                  ; text
                      " is used to detect collisions. Controls: giving speed to the body \
                       by clicking on the circle, zooming through the mouse wheel, \
                       moving the scene by holding the LMB. Calculations can be \
                       performed on the client (Offline) or on the server (Online). In \
                       the second case, several people can interact. More details in \
                       the "
                  ; a
                      ~attr:
                        (href
                           "https://raw.githubusercontent.com/prekel/chapgame/master/thesis/thesis.pdf")
                      [ text "bachelor thesis (RU)." ]
                  ; hr ()
                  ; text
                      "Данная модель симулирует движение и столкновения тел, движущихся \
                       равноускоренно (равнозамедленно) под действием силы трения. Для \
                       обнаружения столкновений используется "
                  ; a
                      ~attr:
                        (href
                           "https://ru.wikipedia.org/wiki/%D0%9E%D0%B1%D0%BD%D0%B0%D1%80%D1%83%D0%B6%D0%B5%D0%BD%D0%B8%D0%B5_%D1%81%D1%82%D0%BE%D0%BB%D0%BA%D0%BD%D0%BE%D0%B2%D0%B5%D0%BD%D0%B8%D0%B9#%D0%9E%D0%B1%D0%BD%D0%B0%D1%80%D1%83%D0%B6%D0%B5%D0%BD%D0%B8%D0%B5_%D1%81%D1%82%D0%BE%D0%BB%D0%BA%D0%BD%D0%BE%D0%B2%D0%B5%D0%BD%D0%B8%D0%B9_%D0%B2_%D1%84%D0%B8%D0%B7%D0%B8%D1%87%D0%B5%D1%81%D0%BA%D0%B8%D1%85_%D1%81%D0%B8%D0%BC%D1%83%D0%BB%D1%8F%D1%86%D0%B8%D1%8F%D1%85")
                      [ text "априорный подход" ]
                  ; text
                      ". Управление: придании скорости телу по нажатию на круг, масштаб \
                       через колесо мыши, перемещение сцены зажимая лкм. Вычисления \
                       могут выполняться на клиенте (Offline) или на сервере (Online). \
                       Во втором случае могут взаимодействовать несколько человек. \
                       Подробнее в "
                  ; a
                      ~attr:
                        (href
                           "https://raw.githubusercontent.com/prekel/chapgame/master/thesis/thesis.pdf")
                      [ text "бакалаврской работе (ВКР)." ]
                  ]
              ]
          ]
      else none)
    ]
;;
