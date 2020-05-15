module Main exposing (..)

import Browser
import Cow exposing (Cow)
import Game
import Html exposing (Html)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Platform.Sub
import Random
import Svg exposing (svg)
import Svg.Attributes exposing (height, viewBox, width)


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
        , subscriptions = \_ -> Platform.Sub.none
        }


init : () -> ( Model, Cmd Msg )
init flags =
    ( Menu, Cmd.none )


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
                    ( Menu, Cmd.none )

        ( GameMsg _, Menu ) ->
            ( Menu, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Menu ->
            Html.div [ style "text-align" "center" ]
                [ Html.h1 [] [ Html.text "Is it my cow?" ]
                , Html.img [ src "cowhead.svg" ] []
                , Html.br [] []
                , Html.br [] []
                , Html.button [ onClick StartGame ] [ Html.text "Start" ]
                ]

        Game game ->
            Html.map GameMsg <| Game.view game
