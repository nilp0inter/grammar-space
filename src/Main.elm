module Main exposing (main)

import Animator
import Animator.Inline
import Browser
import Color
import Html exposing (Html, div, h1, p, text)
import Html.Attributes as Attr
import Html.Events as Events
import Time


type alias Model =
    { active : Animator.Timeline Bool
    }


type Msg
    = Tick Time.Posix
    | Toggle


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watchingWith
            .active
            (\newActive model -> { model | active = newActive })
            (always False)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { active = Animator.init False }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( model |> Animator.update newTime animator
            , Cmd.none
            )

        Toggle ->
            ( { model
                | active =
                    model.active
                        |> Animator.go Animator.slowly
                            (not (Animator.current model.active))
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Animator.toSubscription Tick model animator


view : Model -> Html Msg
view model =
    div [ Attr.class "flex flex-col items-center justify-center min-h-screen gap-8" ]
        [ h1 [ Attr.class "text-5xl font-bold tracking-tight" ]
            [ text "Grammar Space" ]
        , p [ Attr.class "text-slate-400 text-lg" ]
            [ text "Click the shape below" ]
        , div
            [ Events.onClick Toggle
            , Animator.Inline.opacity model.active <|
                \state ->
                    if state then
                        Animator.at 1

                    else
                        Animator.at 0.4
            , Animator.Inline.backgroundColor model.active <|
                \state ->
                    if state then
                        Color.rgb255 99 102 241

                    else
                        Color.rgb255 71 85 105
            , Animator.Inline.scale model.active <|
                \state ->
                    if state then
                        Animator.at 1.1

                    else
                        Animator.at 1.0
            , Attr.class "w-32 h-32 rounded-2xl cursor-pointer flex items-center justify-center select-none"
            ]
            [ p [ Attr.class "text-white text-sm font-medium" ]
                [ text
                    (if Animator.current model.active then
                        "Active"

                     else
                        "Inactive"
                    )
                ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
