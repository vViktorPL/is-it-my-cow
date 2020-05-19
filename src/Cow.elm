module Cow exposing (Cow, cowHeight, cowWidth, moveTo, random, setSize, view)

import Html exposing (Html)
import Random
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (height, style, viewBox, width)
import Svg.Events exposing (onClick)
import SvgBlob exposing (SvgBlob)


type alias CowPatch =
    ( SvgBlob, ( Float, Float ) )


type Cow
    = Cow
        { patches : List CowPatch
        , position : ( Float, Float )
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
                    , position = ( 0, 0 )
                    , size = size
                    , action = Stand
                    }
            )


moveTo : ( Float, Float ) -> Cow -> Cow
moveTo position (Cow data) =
    Cow { data | position = position }


setSize : ( Float, Float ) -> Cow -> Cow
setSize size (Cow data) =
    Cow { data | size = size }


view : msg -> Cow -> Svg msg
view onClickMsg (Cow { patches, position, size, action }) =
    let
        ( x, y ) =
            position

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
    --    svg
    --        [ width (String.fromFloat cowWidth)
    --        , height (String.fromFloat cowHeight)
    --        , viewBox ("0 0 " ++ String.fromFloat cowWidth ++ " " ++ String.fromFloat cowHeight)
    --        ]
    svg
        [ Svg.Attributes.x <| String.fromFloat x
        , Svg.Attributes.y <| String.fromFloat y
        , Svg.Attributes.width <| String.fromFloat width
        , Svg.Attributes.height <| String.fromFloat height
        , Svg.Attributes.viewBox ("0 0 " ++ String.fromFloat cowWidth ++ " " ++ String.fromFloat cowHeight)
        , style "cursor: pointer"
        , onClick onClickMsg
        ]
        [ cowBodyClipPath, cowBody, cowHead ]
