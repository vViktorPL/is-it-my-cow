module Cow exposing (Cow, Msg(..), cowHeight, cowWidth, getTargetPosition, headTo, random, setScale, teleportTo, update, view, viewStatic)

import Html.Attributes
import Html.Events
import Json.Decode
import Random
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (height, width)
import Svg.Events exposing (onClick)
import SvgBlob exposing (SvgBlob)


type alias CowPatch =
    ( SvgBlob, ( Float, Float ) )


type Cow
    = Cow
        { patches : List CowPatch
        , targetPosition : ( Float, Float )
        , scale : Float
        , action : CowAction
        }


type CowAction
    = Idle
    | Walk Direction


type Direction
    = Left
    | Right


type LegAnimation
    = Stand
    | GoLeft Float


type Msg
    = Clicked
    | ReachedTarget


onTransitionEnd msg =
    Html.Events.on "transitionend" (Json.Decode.succeed msg)


onTransitionCancel msg =
    Html.Events.on "transitioncancel" (Json.Decode.succeed msg)


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


random : Float -> Random.Generator Cow
random scale =
    Random.int minPatches maxPatches
        |> Random.andThen (\patchesCount -> Random.list patchesCount randomPatch)
        |> Random.map
            (\patches ->
                Cow
                    { patches = patches
                    , targetPosition = ( 0, 0 )
                    , scale = scale
                    , action = Idle
                    }
            )


headTo : ( Float, Float ) -> Cow -> Cow
headTo position (Cow data) =
    let
        ( tX, tY ) =
            position

        ( pX, pY ) =
            data.targetPosition

        ( dX, dY ) =
            ( tX - pX, tY - pY )

        direction =
            if dX <= 0 then
                Left

            else
                Right

        -- TODO: up/down
    in
    Cow { data | targetPosition = position, action = Walk direction }


teleportTo : ( Float, Float ) -> Cow -> Cow
teleportTo position (Cow data) =
    Cow { data | targetPosition = position, action = Idle }


getTargetPosition : Cow -> ( Float, Float )
getTargetPosition (Cow data) =
    data.targetPosition


setScale : Float -> Cow -> Cow
setScale newScale (Cow data) =
    Cow { data | scale = newScale }


view : Cow -> Svg Msg
view ((Cow { patches, targetPosition, scale, action }) as model) =
    let
        ( targetX, targetY ) =
            targetPosition
    in
    Svg.g
        [ onTransitionEnd ReachedTarget
        , Html.Attributes.style "transition" "transform linear 3s"
        , Html.Attributes.style "transform" <|
            "translate("
                ++ String.fromFloat targetX
                ++ "px,"
                ++ String.fromFloat targetY
                ++ "px)"
        ]
        [ viewStatic model
        ]


viewStatic : Cow -> Svg Msg
viewStatic (Cow { patches, targetPosition, scale, action }) =
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
                    (\( blob, ( patchX, patchY ) ) ->
                        Svg.g
                            [ Svg.Attributes.transform <| "translate(" ++ String.fromFloat patchX ++ " " ++ String.fromFloat patchY ++ ")"
                            , Svg.Attributes.fill "black"
                            ]
                            [ SvgBlob.view blob ]
                    )

        rightLegAnim : LegAnimation
        rightLegAnim =
            case action of
                Idle ->
                    Stand

                Walk _ ->
                    GoLeft 0

        leftLegAnim : LegAnimation
        leftLegAnim =
            case action of
                Idle ->
                    Stand

                Walk _ ->
                    GoLeft 0.5
    in
    Svg.g
        [ Html.Attributes.style "transform" <|
            "scale("
                ++ String.fromFloat scale
                ++ ")"
        , Html.Attributes.style "cursor" "pointer"
        , onClick Clicked
        ]
        [ Svg.g
            -- Flip cow when walking right
            (if action == Walk Right then
                [ Svg.Attributes.transform <| "scale(-1 1) translate(" ++ String.fromFloat -cowWidth ++ " 0)"
                ]

             else
                []
            )
            [ cowBodyClipPath
            , viewLeg ( cowBodyCX - cowBodyRX * 0.8, cowBodyCY * 0.9 ) rightLegAnim
            , viewLeg ( cowBodyCX + cowBodyRX * 0.6, cowBodyCY * 0.9 ) rightLegAnim
            , cowBody
            , viewLeg ( cowBodyCX - cowBodyRX * 0.6, cowBodyCY ) leftLegAnim
            , viewLeg ( cowBodyCX + cowBodyRX * 0.8, cowBodyCY ) leftLegAnim
            , cowHead
            ]
        ]


cowStepSize =
    cowBodyRY * 0.2


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


update : Msg -> Cow -> Cow
update msg (Cow state) =
    case msg of
        ReachedTarget ->
            Cow { state | action = Idle }

        _ ->
            Cow state
