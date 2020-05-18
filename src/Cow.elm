module Cow exposing (Cow, cowHeight, cowWidth, goLeft, random, view)

import Html exposing (Html)
import Random
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (height, viewBox, width)
import SvgBlob exposing (SvgBlob)


type alias CowPatch =
    ( SvgBlob, ( Float, Float ) )


type CowState
    = Standing
    | GoingLeft
    | GoingRight


type Cow
    = Cow CowState (List CowPatch)


minPatches =
    4


maxPatches =
    8


minPatchSize =
    cowWidth / 5


maxPatchSize =
    cowWidth / 3


cowWidth =
    320


cowHeight =
    240


cowBodyCX =
    cowWidth * 0.6


cowBodyCY =
    cowHeight * 0.46


cowBodyRX =
    cowWidth * 0.38


cowBodyRY =
    cowHeight * 0.3


randomPatch : Random.Generator CowPatch
randomPatch =
    let
        randomPosition =
            Random.pair
                (Random.float (cowBodyCX - cowBodyRX - minPatchSize) (cowBodyCX + cowBodyRX - minPatchSize))
                (Random.float (cowBodyCY - cowBodyRY - minPatchSize) (cowBodyCY + cowBodyRY - minPatchSize))

        randomSize =
            Random.float minPatchSize maxPatchSize

        randomNodeCount =
            Random.int minPatches maxPatches
    in
    Random.map3
        SvgBlob.random
        randomSize
        randomSize
        randomNodeCount
        |> Random.andThen (\randomBlob -> Random.pair randomBlob randomPosition)


random : Random.Generator Cow
random =
    Random.int minPatches maxPatches
        |> Random.andThen (\patchesCount -> Random.list patchesCount randomPatch)
        |> Random.map (Cow Standing)


cowStepSize =
    cowBodyRY * 0.2


type LegAnimation
    = Stand
    | GoLeft Float
    | GoRight Float


viewLeg : ( Float, Float ) -> LegAnimation -> Svg msg
viewLeg ( x, y ) animation =
    let
        cy =
            y + cowBodyRY * 0.7
    in
    Svg.g []
        [ Svg.ellipse
            [ Svg.Attributes.ry <| String.fromFloat (cowBodyRY * 0.8)
            , Svg.Attributes.rx <| String.fromFloat (cowBodyRY * 0.15)
            , Svg.Attributes.cx <| String.fromFloat x
            , Svg.Attributes.cy <| String.fromFloat cy
            , Svg.Attributes.style "fill: white; stroke: black"
            ]
            (case animation of
                GoLeft delay ->
                    [ Svg.animateMotion
                        [ Svg.Attributes.path <|
                            pathCmds
                                [ ( 'M', [ ( 0, 0 ) ] )
                                , ( 'C', [ ( 0, 0 ), ( -cowStepSize, -cowStepSize ), ( -cowStepSize, 0 ) ] )
                                , ( 'L', [ ( 0, 0 ) ] )
                                ]
                        , Svg.Attributes.dur "1s"
                        , Svg.Attributes.repeatCount "indefinite"
                        , Svg.Attributes.begin (String.fromFloat delay ++ "s")
                        ]
                        []
                    ]

                _ ->
                    []
            )
        ]


pathCmds : List ( Char, List ( Float, Float ) ) -> String
pathCmds cmds =
    cmds
        |> List.map (\( cmd, points ) -> pathCmd cmd points)
        |> String.join " "


pathCmd : Char -> List ( Float, Float ) -> String
pathCmd cmd points =
    String.fromChar cmd
        ++ (points
                |> List.map (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat y)
                |> String.join " "
           )


view : Cow -> Svg msg
view (Cow state patches) =
    let
        cowHead =
            Svg.image
                [ Svg.Attributes.xlinkHref "cowhead.svg"
                , Svg.Attributes.width "120"
                , Svg.Attributes.height "120"
                ]
                []

        bodyCX =
            cowBodyCX |> String.fromFloat |> Svg.Attributes.cx

        bodyCY =
            cowBodyCY |> String.fromFloat |> Svg.Attributes.cy

        bodyRX =
            cowBodyRX |> String.fromFloat |> Svg.Attributes.rx

        bodyRY =
            cowBodyRY |> String.fromFloat |> Svg.Attributes.ry

        cowBodyClipPath =
            Svg.clipPath [ Svg.Attributes.id "body" ]
                [ Svg.ellipse [ bodyCX, bodyCY, bodyRX, bodyRY ] [] ]

        cowBody =
            Svg.g [ Svg.Attributes.clipPath "url(#body)" ]
                (Svg.ellipse
                    [ bodyCX
                    , bodyCY
                    , bodyRX
                    , bodyRY
                    , Svg.Attributes.fill "white"
                    , Svg.Attributes.stroke "black"
                    ]
                    []
                    :: patchInstances
                )

        patchInstances =
            patches
                |> List.map
                    (\( blob, ( x, y ) ) ->
                        Svg.g
                            [ Svg.Attributes.transform ("translate(" ++ String.fromFloat x ++ " " ++ String.fromFloat y ++ ")")
                            , Svg.Attributes.fill "black"
                            ]
                            [ SvgBlob.view blob ]
                    )

        rightLegAnim =
            case state of
                Standing ->
                    Stand

                GoingLeft ->
                    GoLeft 0.5

                GoingRight ->
                    GoRight 0.5

        leftLegAnim =
            case state of
                Standing ->
                    Stand

                GoingLeft ->
                    GoLeft 0

                GoingRight ->
                    GoRight 0
    in
    --    svg
    --        [ width (String.fromFloat cowWidth)
    --        , height (String.fromFloat cowHeight)
    --        , viewBox ("0 0 " ++ String.fromFloat cowWidth ++ " " ++ String.fromFloat cowHeight)
    --        ]
    Svg.g
        []
        [ cowBodyClipPath
        , viewLeg ( cowBodyCX - cowBodyRX * 0.8, cowBodyCY * 0.9 ) rightLegAnim
        , viewLeg ( cowBodyCX + cowBodyRX * 0.6, cowBodyCY * 0.9 ) rightLegAnim
        , cowBody
        , viewLeg ( cowBodyCX - cowBodyRX * 0.6, cowBodyCY ) leftLegAnim
        , viewLeg ( cowBodyCX + cowBodyRX * 0.8, cowBodyCY ) leftLegAnim
        , cowHead
        ]


goLeft : Cow -> Cow
goLeft (Cow _ patches) =
    Cow GoingLeft patches
