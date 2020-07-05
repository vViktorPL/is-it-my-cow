module Timeout exposing
    ( Msg
    , Timeout
    , fromSeconds
    , infinite
    , reset
    , start
    , subscription
    , timeLeft
    , toString
    , update
    , view
    )

import Html exposing (Html)
import Html.Attributes exposing (style)
import Task
import Time exposing (Posix)


type Msg
    = TimeTick Posix


type Timeout
    = StoppedTimeout Int
    | RunningTimeout
        { interval : Int
        , startTime : Posix
        , currentTime : Posix
        }
    | Infinite


fromSeconds : Int -> Timeout
fromSeconds secs =
    StoppedTimeout secs


infinite =
    Infinite


reset : Timeout -> Timeout
reset timeout =
    case timeout of
        RunningTimeout { interval } ->
            StoppedTimeout interval

        _ ->
            timeout


start : (Timeout -> msg) -> Timeout -> Cmd msg
start msg timeout =
    case reset timeout of
        StoppedTimeout secs ->
            Time.now
                |> Task.map
                    (\startTime ->
                        RunningTimeout
                            { interval = secs
                            , startTime = startTime
                            , currentTime = startTime
                            }
                    )
                |> Task.perform msg

        _ ->
            timeout
                |> Task.succeed
                |> Task.perform msg


update : Msg -> Timeout -> Timeout
update (TimeTick currentTime) model =
    case model of
        RunningTimeout modelInternals ->
            RunningTimeout { modelInternals | currentTime = currentTime }

        _ ->
            model


toString : Timeout -> String
toString model =
    let
        secs =
            timeLeft model
    in
    if isInfinite secs then
        "∞"

    else
        String.fromFloat secs


view : Timeout -> Html msg
view model =
    if timeLeft model <= 3 then
        Html.span [ style "color" "red" ]
            [ Html.text <| toString model ]

    else
        Html.text <| toString model


posixToSecs : Posix -> Int
posixToSecs time =
    Time.posixToMillis time // 1000


timeLeft : Timeout -> Float
timeLeft model =
    case model of
        StoppedTimeout secs ->
            toFloat secs

        RunningTimeout { interval, startTime, currentTime } ->
            interval
                - posixToSecs currentTime
                + posixToSecs startTime
                |> max 0
                |> toFloat

        Infinite ->
            1 / 0


subscription : Timeout -> Sub Msg
subscription model =
    case model of
        RunningTimeout _ ->
            Time.every 500 TimeTick

        _ ->
            Sub.none
