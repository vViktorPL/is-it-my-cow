module Game exposing (Model, Msg, startGame, subscription, update, view)

import Cow exposing (Cow)
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
    | GenerateNextLevel
    | NextLevel Level
    | TryAgain
    | ExitGame
    | DoSomeRandomAction
    | AnimateCow Int ( Float, Float )
    | CowMsg Cow Cow.Msg
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


gameLevelMap : (Level -> Level) -> Model -> Model
gameLevelMap f (Game state) =
    Game { state | level = f state.level }


myCowMap : (Cow -> Cow) -> Level -> Level
myCowMap f ((Level cows) as level) =
    let
        myCowUpdated =
            f <| getMyCow level
    in
    Level <| List.Nonempty.replaceHead myCowUpdated cows


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


cowMissed : Model -> ( Maybe Model, Cmd Msg )
cowMissed (Game state) =
    if state.lives == 1 then
        ( Just <| Game { state | screen = GameOver }, Cmd.none )

    else
        ( Just <| Game { state | screen = Failure, lives = state.lives - 1 }, Cmd.none )


update : Msg -> Model -> ( Maybe Model, Cmd Msg )
update msg ((Game state) as model) =
    case msg of
        Ready ->
            ( Just <| Game { state | screen = FindMyCow }
            , Music.playSong "Ludwigs_Steirische_Gaudi_-_01_-_Rosies_Dance_Polka_ID_22.mp3"
            )

        CowMsg cow Cow.Clicked ->
            case ( state.level, state.screen ) of
                ( Level cows, FindMyCow ) ->
                    if List.Nonempty.head cows == cow then
                        cowFound model

                    else
                        cowMissed model

                _ ->
                    ( Just <| model, Cmd.none )

        CowMsg cow innerMsg ->
            ( Just <|
                gameLevelMap
                    (\(Level cows) ->
                        List.Nonempty.map
                            (\c ->
                                if c == cow then
                                    Cow.update innerMsg c

                                else
                                    c
                            )
                            cows
                            |> Level
                    )
                    model
            , Cmd.none
            )

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
            Random.Nonempty.nonempty cowsPerLevel (Cow.random levelCowSizeRatio)
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
                        ]
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

        Failure ->
            Html.div
                []
                [ viewCows level
                , viewCloud
                    [ Html.p [] [ Html.text "Oh no! It's not your cow! 😔" ]
                    , Html.button [ onClick TryAgain ] [ Html.text "Try again" ]
                    ]
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
subscription (Game { screen }) =
    case screen of
        FindMyCow ->
            Time.every 3000 (always DoSomeRandomAction)

        _ ->
            Sub.none


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
