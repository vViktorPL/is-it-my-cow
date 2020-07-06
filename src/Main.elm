module Main exposing (..)

import Browser
import Game
import Html exposing (Html)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Music
import Platform.Sub


type Model
    = Menu Bool
    | Game Bool Game.Model


type Msg
    = StartGame
    | ToggleMusic
    | GameInitialized Bool Game.Model
    | GameMsg Game.Msg


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Bool -> ( Model, Cmd Msg )
init music =
    ( Menu music
    , if music then
        playTitleSong

      else
        Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( StartGame, Menu music ) ->
            ( model, Game.startGame (GameInitialized music) music )

        ( GameInitialized music game, _ ) ->
            ( Game music game, Cmd.none )

        ( ToggleMusic, Menu music ) ->
            ( Menu <| not music
            , if music then
                Music.stop ()

              else
                playTitleSong
            )

        ( GameMsg gameMsg, Game music game ) ->
            case Game.update gameMsg game of
                ( Just newGameModel, cmd ) ->
                    ( Game music newGameModel, Cmd.map GameMsg cmd )

                _ ->
                    init music

        _ ->
            ( model, Cmd.none )


playTitleSong =
    Music.playSong "Ludwigs_Steirische_Gaudi_-_11_-_Lucky_Peak_Walz_ID_24.mp3"


view : Model -> Html Msg
view model =
    case model of
        Menu music ->
            Html.div [ style "text-align" "center" ]
                [ Html.h1 [] [ Html.text "Is it my cow?" ]
                , Html.img [ src "cowhead.svg", class "swing" ] []
                , Html.br [] []
                , Html.br [] []
                , Html.button [ onClick StartGame ] [ Html.text "▶️ Start" ]
                , Html.br [] []
                , Html.br [] []
                , Html.button [ onClick ToggleMusic ]
                    [ Html.text <|
                        "🎶 Music: "
                            ++ (if music then
                                    "On"

                                else
                                    "Off"
                               )
                    ]
                ]

        Game _ game ->
            Html.map GameMsg <| Game.view game


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Game _ game ->
            Sub.map GameMsg (Game.subscription game)

        _ ->
            Sub.none
