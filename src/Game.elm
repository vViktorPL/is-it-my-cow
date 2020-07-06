module Game exposing (Model, Msg, startGame, subscription, update, view)

import Cow exposing (Cow)
import Difficulty exposing (LevelDifficultyParams)
import Grid
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List.Nonempty exposing (Nonempty)
import Music
import Random
import Random.List
import Random.Nonempty
import Svg exposing (svg)
import Svg.Attributes exposing (height, preserveAspectRatio, viewBox, width, xlinkHref)
import Time exposing (Posix)
import Timeout exposing (Timeout)


type alias Level =
    { number : Int
    , cows : Nonempty Cow
    , myCowScreenTimeout : Timeout
    , findMyCowTimeout : Timeout
    }


type GameScreen
    = MyCow
    | FindMyCow
    | Success
    | Failure FailureReason
    | GameOver


type FailureReason
    = WrongCow
    | TimesUp


type Model
    = Game
        { lives : Int
        , score : Int
        , level : Level
        , screen : GameScreen
        , music : Bool
        }


type Msg
    = Ready
    | ShowLevel Timeout
    | GenerateNextLevel
    | NextLevel Level
    | TryAgain
    | ShowMyCowScreen Timeout
    | ExitGame
    | DoSomeRandomAction
    | AnimateCow Int ( Float, Float )
    | CowMsg Cow Cow.Msg
    | Idle
    | FindMyCowTimeoutMsg Timeout.Msg
    | MyCowScreenTimeoutMsg Timeout.Msg


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


gameLevelMap : (Level -> Level) -> Model -> Model
gameLevelMap f (Game state) =
    Game { state | level = f state.level }


myCowMap : (Cow -> Cow) -> Level -> Level
myCowMap f level =
    { level | cows = nonemptyMapHead f level.cows }


nonemptyMapHead : (a -> a) -> Nonempty a -> Nonempty a
nonemptyMapHead f list =
    List.Nonempty.replaceHead (f <| List.Nonempty.head list) list


cowSlot : Cow -> ( Int, Int )
cowSlot cow =
    let
        ( x, y ) =
            Cow.getTargetPosition cow
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


cowFound : Model -> ( Maybe Model, Cmd Msg )
cowFound (Game state) =
    ( Just <| Game { state | screen = Success, score = state.score + 10 }, Cmd.none )


cowMissed : FailureReason -> Model -> ( Maybe Model, Cmd Msg )
cowMissed failureReason (Game state) =
    if state.lives == 1 then
        ( Just <| Game { state | screen = GameOver }, Cmd.none )

    else
        ( Just <| Game { state | screen = Failure failureReason, lives = state.lives - 1 }, Cmd.none )


update : Msg -> Model -> ( Maybe Model, Cmd Msg )
update msg ((Game state) as model) =
    case msg of
        Ready ->
            ( Just model
            , Timeout.start ShowLevel state.level.findMyCowTimeout
            )

        ShowLevel timeout ->
            ( Game { state | screen = FindMyCow }
                |> gameLevelMap
                    (\level ->
                        { level | findMyCowTimeout = timeout }
                    )
                |> Just
            , Music.playSong "Ludwigs_Steirische_Gaudi_-_01_-_Rosies_Dance_Polka_ID_22.mp3"
            )

        CowMsg cow Cow.Clicked ->
            case state.screen of
                FindMyCow ->
                    if List.Nonempty.head state.level.cows == cow then
                        cowFound model

                    else
                        cowMissed WrongCow model

                _ ->
                    ( Just <| model, Cmd.none )

        CowMsg cow innerMsg ->
            ( Just <|
                gameLevelMap
                    (\level ->
                        { level
                            | cows =
                                List.Nonempty.map
                                    (\c ->
                                        if c == cow then
                                            Cow.update innerMsg c

                                        else
                                            c
                                    )
                                    level.cows
                        }
                    )
                    model
            , Cmd.none
            )

        GenerateNextLevel ->
            ( Just <| Game state
            , state.level
                |> getLevelNumber
                |> (+) 1
                |> randomLevel
                |> Random.generate NextLevel
            )

        NextLevel level ->
            ( Just <| Game { state | level = level }
            , Timeout.start ShowMyCowScreen level.findMyCowTimeout
            )

        TryAgain ->
            ( Just model
            , Timeout.start ShowMyCowScreen state.level.myCowScreenTimeout
            )

        ShowMyCowScreen timeout ->
            ( Game { state | screen = MyCow }
                |> gameLevelMap
                    (\level ->
                        { level
                            | myCowScreenTimeout = timeout
                        }
                    )
                |> Just
            , Cmd.none
            )

        DoSomeRandomAction ->
            ( Just model
            , Random.int 0 (List.Nonempty.length state.level.cows)
                |> Random.andThen
                    (\cowIndex ->
                        let
                            cow =
                                List.Nonempty.get cowIndex state.level.cows

                            restCows =
                                List.filter ((/=) cow) (List.Nonempty.toList state.level.cows)

                            possibleMoves =
                                cowPossibleMoves cow restCows
                        in
                        Random.List.choose possibleMoves
                            |> Random.map Tuple.first
                            |> Random.map (Maybe.map (\position -> ( cowIndex, position )))
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
                    (\level ->
                        { level
                            | cows =
                                List.Nonempty.indexedMap
                                    (\index cow ->
                                        if index == cowIndex then
                                            Cow.headTo targetPosition cow

                                        else
                                            cow
                                    )
                                    level.cows
                        }
                    )
                |> Just
            , Cmd.none
            )

        FindMyCowTimeoutMsg timeoutMsg ->
            let
                updatedTimeout =
                    Timeout.update timeoutMsg state.level.findMyCowTimeout
            in
            if Timeout.timeLeft updatedTimeout <= 0 && state.screen == FindMyCow then
                cowMissed TimesUp model

            else
                ( Just <|
                    gameLevelMap
                        (\level ->
                            { level | findMyCowTimeout = updatedTimeout }
                        )
                        model
                , Cmd.none
                )

        MyCowScreenTimeoutMsg timeoutMsg ->
            let
                updatedTimeout =
                    Timeout.update timeoutMsg state.level.myCowScreenTimeout
            in
            if Timeout.timeLeft updatedTimeout <= 0 && state.screen == MyCow then
                ( Just model
                , Timeout.start ShowLevel state.level.findMyCowTimeout
                )

            else
                ( Just <|
                    gameLevelMap
                        (\level ->
                            { level | myCowScreenTimeout = Timeout.update timeoutMsg level.myCowScreenTimeout }
                        )
                        model
                , Cmd.none
                )

        Idle ->
            ( Just model
            , Cmd.none
            )

        ExitGame ->
            ( Nothing, Cmd.none )


randomLevel : Int -> Random.Generator Level
randomLevel lvlNumber =
    let
        { randomCowsCount, mutatedCowsCount, mutantSimilarity, patchesPerCow, levelTimeout, myCowScreenTimeout } =
            Difficulty.forLevelNumber lvlNumber

        cols =
            floor (meadowWidth / cellWidth)

        rows =
            floor (meadowHeight / cellHeight)

        cowPositionSlots =
            Grid.grid2D cols rows

        randomCowPositions =
            cowPositionSlots
                |> Grid.pickRandom2DGridCells (randomCowsCount + mutatedCowsCount)
                |> Random.map
                    (List.Nonempty.map
                        (\( slotX, slotY ) ->
                            ( toFloat slotX * cellWidth + (screenWidth - meadowWidth)
                            , toFloat slotY * cellHeight + (screenHeight - meadowHeight)
                            )
                        )
                    )

        randomCows =
            Random.Nonempty.nonempty (randomCowsCount - 1) (Cow.random patchesPerCow)
                |> Random.andThen
                    (\baseCows ->
                        let
                            randomMutatedCows : Random.Generator (Nonempty Cow)
                            randomMutatedCows =
                                Random.Nonempty.nonempty
                                    (mutatedCowsCount - 1)
                                    (Random.pair
                                        (Random.float 0 1)
                                        (Random.Nonempty.choose baseCows)
                                        |> Random.map
                                            (\( seed, cow ) ->
                                                Cow.mutate mutantSimilarity seed cow
                                            )
                                    )
                        in
                        randomMutatedCows
                            |> Random.map (List.Nonempty.append baseCows)
                    )
    in
    Random.pair randomCowPositions randomCows
        |> Random.map
            (\( positions, cows ) ->
                { number = lvlNumber
                , cows =
                    cows
                        |> List.Nonempty.zip positions
                        |> List.Nonempty.map
                            (\( position, cow ) ->
                                cow
                                    |> Cow.teleportTo position
                                    |> Cow.setScale levelCowSizeRatio
                            )
                , myCowScreenTimeout =
                    myCowScreenTimeout
                , findMyCowTimeout =
                    levelTimeout
                }
            )


init : Bool -> Level -> Model
init music level =
    Game
        { lives = 3
        , score = 0
        , level = level
        , screen = MyCow
        , music = music
        }


startGame : (Model -> msg) -> Bool -> Cmd msg
startGame tagger music =
    randomLevel 1
        |> Random.map (init music)
        |> Random.generate tagger


getMyCow : Level -> Cow
getMyCow { cows } =
    List.Nonempty.head cows


viewReadyButton : Level -> Html Msg
viewReadyButton { myCowScreenTimeout } =
    let
        label =
            if myCowScreenTimeout == Timeout.infinite then
                "I'm ready!"

            else
                "I'm ready! (" ++ Timeout.toString myCowScreenTimeout ++ ")"
    in
    Html.button
        [ onClick Ready
        , style "position" "absolute"
        , style "bottom" "220px"
        ]
        [ Html.text label ]


view : Model -> Html Msg
view (Game { lives, score, level, screen }) =
    case screen of
        MyCow ->
            Html.div
                [ style "background-image" "url('barn-landscape.svg')"
                , style "background-size" "cover"
                , style "width" <| String.fromInt screenWidth ++ "px"
                , style "height" <| String.fromInt screenHeight ++ "px"
                , style "text-align" "center"
                , style "position" "relative"
                ]
                [ Html.h2
                    [ style "position" "absolute"
                    , style "left" "0"
                    , style "right" "0"
                    ]
                    [ Html.text "This is your cow:" ]
                , level
                    |> getMyCow
                    |> Cow.teleportTo ( 0, 0 )
                    |> Cow.setScale 1
                    |> Cow.viewStatic
                    |> Html.map
                        (\innerMsg ->
                            if innerMsg == Cow.Clicked then
                                Ready

                            else
                                Idle
                        )
                    |> List.singleton
                    |> Svg.svg
                        [ Svg.Attributes.width <| String.fromFloat Cow.cowWidth
                        , Svg.Attributes.height <| String.fromFloat Cow.cowHeight
                        , style "position" "absolute"
                        , style "left" "250px"
                        , style "bottom" "0"
                        ]
                , viewReadyButton level
                ]

        FindMyCow ->
            Html.div
                []
                [ viewCows level
                , Html.div [ style "position" "absolute", style "top" "20px", style "left" "20px" ]
                    [ Html.h2 [] [ Html.text "Which one is your cow?" ]
                    , viewStatusBar lives score level.findMyCowTimeout
                    ]
                ]

        Success ->
            Html.div
                []
                [ level
                    |> myCowMap Cow.happyJump
                    |> viewCows
                , viewCloud
                    [ Html.p [] [ Html.text "You've found it! 🎉" ]
                    , Html.button [ onClick GenerateNextLevel ] [ Html.text "Next level" ]
                    ]
                ]

        Failure reason ->
            let
                reasonText =
                    case reason of
                        WrongCow ->
                            "Oh no! It's not your cow! 😔"

                        TimesUp ->
                            "Time's up! ✋"
            in
            Html.div
                []
                [ viewCows level
                , viewCloud
                    [ Html.p [] [ Html.text reasonText ]
                    , Html.button [ onClick TryAgain ] [ Html.text "Try again" ]
                    ]
                ]

        GameOver ->
            Html.div
                [ style "width" "100%"
                , style "height" "100%"
                , style "text-align" "center"
                , style "background" "black"
                , style "color" "white"
                , style "padding-top" "150px"
                ]
                [ Html.h2 [] [ Html.text "Game Over \u{1F937}" ]
                , Html.p [] [ Html.text "Your score is: ", Html.strong [] [ Html.text (String.fromInt score) ] ]
                , Html.button [ onClick ExitGame ] [ Html.text "Go back to the menu" ]
                ]


viewStatusBar : Int -> Int -> Timeout -> Html Msg
viewStatusBar lives score timeout =
    Html.div []
        [ Html.p [] [ Html.text ("Lives: " ++ String.repeat lives "️️❤️") ]
        , Html.p [] [ Html.text "💎 Score: ", Html.strong [] [ Html.text (String.fromInt score) ] ]
        , Html.p [] [ Html.text "⏱️ Time: ", Html.strong [] [ Timeout.view timeout ] ]
        ]


viewCows : Level -> Html Msg
viewCows { cows } =
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
                |> Cow.view
                |> Html.map (CowMsg (List.Nonempty.head cows))

        restCows =
            cows
                |> List.Nonempty.tail
                |> List.map (\cow -> Cow.view cow |> Html.map (CowMsg cow))
    in
    svg
        [ style "width" "100%"
        , style "height" "100%"
        , viewBox ([ 0, 0, screenWidth, screenHeight ] |> List.map String.fromInt |> String.join " ")
        , preserveAspectRatio "xMidYMax meet"
        ]
        (meadow :: List.reverse (myCow :: restCows))


subscription : Model -> Sub Msg
subscription (Game { screen, level }) =
    Sub.batch
        [ case screen of
            FindMyCow ->
                Time.every 3000 (always DoSomeRandomAction)

            _ ->
                Sub.none
        , Timeout.subscription level.findMyCowTimeout
            |> Sub.map FindMyCowTimeoutMsg
        , Timeout.subscription level.myCowScreenTimeout
            |> Sub.map MyCowScreenTimeoutMsg
        ]


viewCloud children =
    Html.div
        [ style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        , style "right" "0"
        , style "height" "30vh"
        , style "background" "url('cloud.svg')"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "background-size" "contain"
        , style "background-repeat" "no-repeat"
        , style "background-position" "center"
        , style "font-size" "3.5vh"
        ]
        [ Html.div [ style "display" "block", style "text-align" "center" ]
            children
        ]


getLevelNumber : Level -> Int
getLevelNumber { number } =
    number
