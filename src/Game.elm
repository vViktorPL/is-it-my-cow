module Game exposing (Model, Msg, startGame, subscription, update, view)

import Animator
import Browser.Events
import Cow exposing (Cow)
import Grid
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List.Nonempty exposing (Nonempty)
import Random
import Random.List
import Random.Nonempty
import Svg exposing (svg)
import Svg.Attributes exposing (height, preserveAspectRatio, viewBox, width, xlinkHref)
import Time


type Level
    = Level (Nonempty Cow)


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
    | Tick Time.Posix
    | DoSomeRandomAction
    | AnimateCow Int ( Float, Float )
    | Idle


screenWidth =
    800


screenHeight =
    600


meadowWidth =
    screenWidth


meadowHeight =
    screenHeight - 220


meadowTop =
    screenHeight - meadowHeight


cellWidth =
    Cow.cowWidth * levelCowSizeRatio


cellHeight =
    Cow.cowHeight * levelCowSizeRatio


levelCowSizeRatio =
    0.5


cowsPerLevel =
    5


animate : Time.Posix -> Model -> Model
animate newTime (Game ({ level } as state)) =
    Game
        { state
            | level =
                case level of
                    Level cows ->
                        cows
                            |> List.Nonempty.map (Animator.update newTime Cow.animator)
                            |> Level
        }


gameLevelMap : (Level -> Level) -> Model -> Model
gameLevelMap f (Game state) =
    Game { state | level = f state.level }


cowSlot : Cow -> ( Int, Int )
cowSlot cow =
    let
        ( x, y ) =
            Cow.getPosition cow
    in
    ( floor (x / cellWidth), floor ((y - meadowTop) / cellHeight) )


slotCoordsToPosition : ( Int, Int ) -> ( Float, Float )
slotCoordsToPosition ( slotX, slotY ) =
    ( toFloat slotX * cellWidth, toFloat slotY * cellHeight + meadowTop )


cowPossibleMoves : Cow -> List Cow -> List ( Float, Float )
cowPossibleMoves cow restCows =
    let
        occupiedSlots =
            List.map cowSlot restCows

        ( sx, sy ) =
            cowSlot cow

        maxSX =
            floor (screenWidth / cellWidth)

        maxSY =
            floor (meadowHeight / cellHeight)
    in
    [ ( sx - 1, sy )
    , ( sx + 1, sy )
    , ( sx, sy - 1 )
    , ( sx, sy + 1 )
    ]
        |> List.filter
            (\( x, y ) -> x >= 0 && x < maxSX && y >= 0 && y < maxSY && not (List.member ( x, y ) occupiedSlots))
        |> List.map slotCoordsToPosition


update : Msg -> Model -> ( Maybe Model, Cmd Msg )
update msg ((Game state) as model) =
    case msg of
        Tick newTime ->
            ( Just <| animate newTime model
            , Cmd.none
            )

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

        DoSomeRandomAction ->
            ( Just model
            , case state.level of
                Level cows ->
                    Random.int 0 (List.Nonempty.length cows)
                        |> Random.andThen
                            (\cowIndex ->
                                let
                                    cow =
                                        List.Nonempty.get cowIndex cows

                                    restCows =
                                        List.filter ((/=) cow) (List.Nonempty.toList cows)

                                    possibleMoves =
                                        cowPossibleMoves cow restCows
                                in
                                if Cow.isIdle cow then
                                    Random.List.choose possibleMoves
                                        |> Random.map Tuple.first
                                        |> Random.map (Maybe.map (\position -> ( cowIndex, position )))

                                else
                                    Random.constant Nothing
                            )
                        |> Random.generate
                            (\maybeCowWithTargetPosition ->
                                case maybeCowWithTargetPosition of
                                    Just ( cow, targetPosition ) ->
                                        AnimateCow cow targetPosition

                                    Nothing ->
                                        Idle
                            )
            )

        AnimateCow cowIndex targetPosition ->
            ( model
                |> gameLevelMap
                    (\(Level cows) ->
                        cows
                            |> List.Nonempty.indexedMap
                                (\index cow ->
                                    if index == cowIndex then
                                        Cow.headTo targetPosition cow

                                    else
                                        cow
                                )
                            |> Level
                    )
                |> Just
            , Cmd.none
            )

        Idle ->
            ( Just model
            , Cmd.none
            )

        ExitGame ->
            ( Nothing, Cmd.none )


randomLevel : Random.Generator Level
randomLevel =
    let
        cols =
            floor (meadowWidth / cellWidth)

        rows =
            floor (meadowHeight / cellHeight)

        cowPositionSlots =
            Grid.grid2D cols rows

        randomCowPositions =
            cowPositionSlots
                |> Grid.pickRandom2DGridCells cowsPerLevel
                |> Random.map
                    (List.Nonempty.map
                        (\( slotX, slotY ) ->
                            ( toFloat slotX * cellWidth + (screenWidth - meadowWidth)
                            , toFloat slotY * cellHeight + (screenHeight - meadowHeight)
                            )
                        )
                    )

        randomCows =
            Random.Nonempty.nonempty cowsPerLevel (Cow.random ( cellWidth, cellHeight ))
    in
    Random.pair randomCowPositions randomCows
        |> Random.map
            (\( positions, cows ) ->
                cows
                    |> List.Nonempty.zip positions
                    |> List.Nonempty.map (\( position, cow ) -> Cow.teleportTo position cow)
                    |> Level
            )


init : Level -> Model
init level =
    Game { lives = 3, score = 0, level = level, screen = MyCow }


startGame : (Model -> msg) -> Cmd msg
startGame tagger =
    randomLevel
        |> Random.map init
        |> Random.generate tagger


getMyCow : Level -> Cow
getMyCow (Level cows) =
    List.Nonempty.head cows


view : Model -> Html Msg
view (Game { lives, score, level, screen }) =
    case screen of
        MyCow ->
            Html.div
                []
                [ Html.h2 [] [ Html.text "This is your cow:" ]
                , level
                    |> getMyCow
                    |> Cow.teleportTo ( 0, 0 )
                    |> Cow.setSize ( Cow.cowWidth, Cow.cowHeight )
                    |> Cow.view Ready
                , Html.button [ onClick Ready ] [ Html.text "I'm ready!" ]
                ]

        FindMyCow ->
            Html.div
                []
                [ viewCows level
                , Html.div [ style "position" "absolute", style "top" "20px", style "left" "20px" ]
                    [ Html.h2 [] [ Html.text "Which one is your cow?" ]
                    , viewStatusBar lives score
                    ]
                ]

        Success ->
            Html.div []
                [ Html.text "You've found it! 🎉"
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
        [ Html.p [] [ Html.text ("Lives: " ++ String.repeat lives "️️❤️") ]
        , Html.p [] [ Html.text "Score: ", Html.strong [] [ Html.text (String.fromInt score) ] ]
        ]


viewCows : Level -> Html Msg
viewCows (Level cows) =
    let
        meadow =
            Svg.image
                [ xlinkHref "meadow.svg"
                , screenWidth |> String.fromInt |> width
                , screenHeight |> String.fromInt |> height
                ]
                []

        myCow =
            cows
                |> List.Nonempty.head
                |> Cow.view Found

        restCows =
            cows
                |> List.Nonempty.tail
                |> List.map (Cow.view Missed)
    in
    svg
        [ style "width" "100%"
        , style "height" "100%"
        , viewBox ([ 0, 0, screenWidth, screenHeight ] |> List.map String.fromInt |> String.join " ")
        , preserveAspectRatio "xMidYMin meet"
        ]
        (meadow :: myCow :: restCows)


subscription : Model -> Sub Msg
subscription (Game { screen }) =
    Sub.batch
        [ Browser.Events.onAnimationFrame Tick
        , case screen of
            FindMyCow ->
                Time.every 3000 (always DoSomeRandomAction)

            _ ->
                Sub.none
        ]
