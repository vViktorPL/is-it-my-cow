module Main exposing (..)

import Browser
import Cow exposing (Cow)
import Html
import Platform.Sub
import Random
import Svg exposing (svg)
import Svg.Attributes exposing (height, viewBox, width)


type alias Model =
    Maybe Cow


type Msg
    = GeneratedCow Cow


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Platform.Sub.none
        }


init : () -> ( Model, Cmd Msg )
init flags =
    ( Nothing, Random.generate GeneratedCow Cow.random )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GeneratedCow cow ->
            ( Just cow, Cmd.none )


view model =
    case model of
        Nothing ->
            Html.text "..."

        Just cow ->
            Cow.view cow
