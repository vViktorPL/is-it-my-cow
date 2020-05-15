module Game exposing (Model, Msg, startGame, update, view)

import Cow exposing (Cow)
import Html exposing (Html)
import Html.Events exposing (onClick)
import Random
import Svg exposing (svg)
import Svg.Attributes exposing (height, viewBox, width)


type alias Position =
    ( Float, Float )


type alias CowWithPosition =
    ( Position, Cow )


type Level
    = Level CowWithPosition (List CowWithPosition)


type GameScreen
    = MyCow
    | FindMyCow
    | Success
    | Failure
    | GameOver


type Model
    = Game
        { lives : Int
        , score : Int
        , level : Level
        , screen : GameScreen
        }


type Msg
    = Ready
    | Found
    | Missed
    | GenerateNextLevel
    | NextLevel Level
    | TryAgain
    | ExitGame


update : Msg -> Model -> ( Maybe Model, Cmd Msg )
update msg (Game state) =
    case msg of
        Ready ->
            ( Just <| Game { state | screen = FindMyCow }, Cmd.none )

        Found ->
            ( Just <| Game { state | screen = Success, score = state.score + 10 }, Cmd.none )

        Missed ->
            if state.lives == 1 then
                ( Just <| Game { state | screen = GameOver }, Cmd.none )

            else
                ( Just <| Game { state | screen = Failure, lives = state.lives - 1 }, Cmd.none )

        GenerateNextLevel ->
            ( Just <| Game state, Random.generate NextLevel randomLevel )

        NextLevel level ->
            ( Just <| Game { state | level = level, screen = MyCow }, Cmd.none )

        TryAgain ->
            ( Just <| Game { state | screen = MyCow }, Cmd.none )

        ExitGame ->
            ( Nothing, Cmd.none )


randomLevel : Random.Generator Level
randomLevel =
    Random.pair
        randomCowWithPosition
        (Random.list 4 randomCowWithPosition)
        |> Random.map (\( myCow, restCows ) -> Level myCow restCows)


randomCowWithPosition : Random.Generator CowWithPosition
randomCowWithPosition =
    Random.pair randomPosition Cow.random


randomPosition : Random.Generator Position
randomPosition =
    Random.pair
        (Random.float 0 500)
        (Random.float 0 500)


init : Level -> Model
init level =
    Game { lives = 3, score = 0, level = level, screen = MyCow }


startGame : (Model -> msg) -> Cmd msg
startGame tagger =
    randomLevel
        |> Random.map init
        |> Random.generate tagger


getCowsWithPosition : Level -> List CowWithPosition
getCowsWithPosition (Level myCow restCows) =
    myCow :: restCows


getMyCow : Level -> Cow
getMyCow (Level myCow _) =
    Tuple.second myCow


view : Model -> Html Msg
view (Game { lives, score, level, screen }) =
    case screen of
        MyCow ->
            Html.div
                []
                [ Html.h2 [] [ Html.text "This is your cow:" ]
                , level
                    |> getMyCow
                    |> Cow.view
                    |> List.singleton
                    |> svg [ width (String.fromFloat Cow.cowWidth), height (String.fromFloat Cow.cowHeight) ]
                , Html.button [ onClick Ready ] [ Html.text "I'm ready!" ]
                ]

        FindMyCow ->
            Html.div
                []
                [ Html.h2 [] [ Html.text "Which one is your cow?" ]
                , viewStatusBar lives score
                , viewCows level
                ]

        Success ->
            Html.div []
                [ Html.text "You've found it!"
                , Html.button [ onClick GenerateNextLevel ] [ Html.text "Next level" ]
                ]

        Failure ->
            Html.div []
                [ Html.text "Oh no! It's not your cow!"
                , Html.button [ onClick TryAgain ] [ Html.text "Try again" ]
                ]

        GameOver ->
            Html.div []
                [ Html.h2 [] [ Html.text "Game Over" ]
                , Html.p [] [ Html.text "Your score is: ", Html.strong [] [ Html.text (String.fromInt score) ] ]
                , Html.button [ onClick ExitGame ] [ Html.text "Go back to the menu" ]
                ]


viewStatusBar : Int -> Int -> Html Msg
viewStatusBar lives score =
    Html.div []
        [ Html.p [] [ Html.text ("Lives: " ++ String.repeat lives "❤️") ]
        , Html.p [] [ Html.text "Score: ", Html.strong [] [ Html.text (String.fromInt score) ] ]
        ]


viewCows : Level -> Html Msg
viewCows (Level myCow restCows) =
    svg
        [ width "800", height "600" ]
        ([ Svg.defs []
            ((myCow :: restCows)
                |> List.map Tuple.second
                |> List.indexedMap
                    (\index cow ->
                        cow
                            |> Cow.view
                            |> List.singleton
                            |> Svg.g [ Svg.Attributes.id ("cow-" ++ String.fromInt index) ]
                    )
            )
         ]
            ++ ((myCow :: restCows)
                    |> List.indexedMap
                        (\index ( ( x, y ), cow ) ->
                            Svg.use
                                [ Svg.Attributes.xlinkHref ("#cow-" ++ String.fromInt index)
                                , Svg.Attributes.x (String.fromFloat x)
                                , Svg.Attributes.y (String.fromFloat y)
                                , onClick
                                    (if cow == Tuple.second myCow then
                                        Found

                                     else
                                        Missed
                                    )
                                ]
                                []
                        )
               )
        )
