module Cow exposing (Cow, random, view)

import Html exposing (Html)
import Random
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (height, viewBox, width)
import SvgBlob exposing (SvgBlob)


type alias CowPatch =
    ( SvgBlob, ( Float, Float ) )


type Cow
    = Cow (List CowPatch)


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
        |> Random.map Cow


view : Cow -> Html msg
view (Cow patches) =
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
    in
    svg
        [ width (String.fromFloat cowWidth)
        , height (String.fromFloat cowHeight)
        , viewBox ("0 0 " ++ String.fromFloat cowWidth ++ " " ++ String.fromFloat cowHeight)
        ]
        [ cowBodyClipPath, cowBody, cowHead ]
