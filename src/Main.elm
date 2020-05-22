module Main exposing (..)

import Browser
import Game
import Html exposing (Html)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import Music
import Platform.Sub


type Model
    = Menu
    | Game Game.Model


type Msg
    = StartGame
    | GameInitialized Game.Model
    | GameMsg Game.Msg


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Menu, playTitleSong )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( StartGame, _ ) ->
            ( model, Game.startGame GameInitialized )

        ( GameInitialized game, _ ) ->
            ( Game game, Cmd.none )

        ( GameMsg gameMsg, Game game ) ->
            case Game.update gameMsg game of
                ( Just newGameModel, cmd ) ->
                    ( Game newGameModel, Cmd.map GameMsg cmd )

                _ ->
                    init ()

        ( GameMsg _, Menu ) ->
            ( Menu, Cmd.none )


playTitleSong =
    Music.playSong "Ludwigs_Steirische_Gaudi_-_11_-_Lucky_Peak_Walz_ID_24.mp3"


view : Model -> Html Msg
view model =
    case model of
        Menu ->
            Html.div [ style "text-align" "center" ]
                [ Html.h1 [] [ Html.text "Is it my cow?" ]
                , Html.img [ src "cowhead.svg", class "swing" ] []
                , Html.br [] []
                , Html.br [] []
                , Html.button [ onClick StartGame ] [ Html.text "Start" ]
                ]

        Game game ->
            Html.map GameMsg <| Game.view game


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Game game ->
            Sub.map GameMsg (Game.subscription game)

        _ ->
            Sub.none
