module Cow exposing (Cow, animator, cowHeight, cowWidth, getPosition, headTo, isIdle, random, setSize, teleportTo, view)

import Animator
import Random
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (height, style, width)
import Svg.Events exposing (onClick)
import SvgBlob exposing (SvgBlob)


type alias CowPatch =
    ( SvgBlob, ( Float, Float ) )


type Cow
    = Cow
        { patches : List CowPatch
        , position : Animator.Timeline ( Float, Float )
        , size : ( Float, Float )
        , action : CowAction
        }


type CowAction
    = Stand
    | Walk ( Float, Float )


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


random : ( Float, Float ) -> Random.Generator Cow
random size =
    Random.int minPatches maxPatches
        |> Random.andThen (\patchesCount -> Random.list patchesCount randomPatch)
        |> Random.map
            (\patches ->
                Cow
                    { patches = patches
                    , position = Animator.init ( 0, 0 )
                    , size = size
                    , action = Stand
                    }
            )


headTo : ( Float, Float ) -> Cow -> Cow
headTo position (Cow data) =
    Cow { data | position = data.position |> Animator.go (Animator.seconds 2) position }


teleportTo : ( Float, Float ) -> Cow -> Cow
teleportTo position (Cow data) =
    Cow { data | position = Animator.init position }


getPosition : Cow -> ( Float, Float )
getPosition (Cow data) =
    Animator.current data.position


isIdle : Cow -> Bool
isIdle (Cow { position }) =
    Animator.previous position == Animator.current position


setSize : ( Float, Float ) -> Cow -> Cow
setSize size (Cow data) =
    Cow { data | size = size }


animator : Animator.Animator Cow
animator =
    Animator.animator
        |> Animator.watching
            (\(Cow { position }) -> position)
            (\newPosition (Cow data) ->
                Cow { data | position = newPosition }
            )


view : msg -> Cow -> Svg msg
view onClickMsg (Cow { patches, position, size, action }) =
    let
        ( width, height ) =
            size

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
                    (\( blob, ( patchX, patchY ) ) ->
                        Svg.g
                            [ Svg.Attributes.transform <| "translate(" ++ String.fromFloat patchX ++ " " ++ String.fromFloat patchY ++ ")"
                            , Svg.Attributes.fill "black"
                            ]
                            [ SvgBlob.view blob ]
                    )
    in
    svg
        [ Animator.linear position (Tuple.first >> Animator.at >> Animator.leaveSmoothly 0.5 >> Animator.arriveSmoothly 0.5) |> String.fromFloat |> Svg.Attributes.x
        , Animator.linear position (Tuple.second >> Animator.at >> Animator.leaveSmoothly 0.5 >> Animator.arriveSmoothly 0.5) |> String.fromFloat |> Svg.Attributes.y
        , Svg.Attributes.width <| String.fromFloat width
        , Svg.Attributes.height <| String.fromFloat height
        , Svg.Attributes.viewBox ("0 0 " ++ String.fromFloat cowWidth ++ " " ++ String.fromFloat cowHeight)
        , style "cursor: pointer"
        , onClick onClickMsg
        ]
        [ cowBodyClipPath
        , viewLeg ( cowBodyCX - cowBodyRX * 0.8, cowBodyCY * 0.9 )
        , viewLeg ( cowBodyCX + cowBodyRX * 0.6, cowBodyCY * 0.9 )
        , cowBody
        , viewLeg ( cowBodyCX - cowBodyRX * 0.6, cowBodyCY )
        , viewLeg ( cowBodyCX + cowBodyRX * 0.8, cowBodyCY )
        , cowHead
        ]


cowStepSize =
    cowBodyRY * 0.2


viewLeg : ( Float, Float ) -> Svg msg
viewLeg ( x, y ) =
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
            []
        ]
