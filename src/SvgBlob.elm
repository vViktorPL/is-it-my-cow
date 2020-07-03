module SvgBlob exposing (SvgBlob, random, size, view)

import Curve exposing (catmullRomClosed)
import Random
import SubPath
import Svg exposing (Svg)
import Svg.Attributes


type alias Point =
    ( Float, Float )


type SvgBlob
    = SvgBlob
        { points : List Point
        , curvedPath : String
        }


followPoint : Point -> Point -> Float -> Point
followPoint ( startX, startY ) ( targetX, targetY ) relativeDistance =
    ( (targetX - startX) * relativeDistance, (targetY - startY) * relativeDistance )


randomBlobPoints : Float -> Float -> Int -> Random.Generator (List Point)
randomBlobPoints width height nodeCount =
    let
        boundaryPerimeter =
            width * 2 + height * 2

        cx =
            width * 0.5

        cy =
            height * 0.5

        nodeLocationToPoint : Float -> Point
        nodeLocationToPoint location =
            let
                p =
                    location * boundaryPerimeter
            in
            if p > width + height + width then
                ( 0, height - (p - width * 2 - height) )

            else if p > width + height then
                ( width - (p - width - height), height )

            else if p > width then
                ( width, p - width )

            else
                ( p, 0 )
    in
    Random.list nodeCount (Random.pair (Random.float 0 1) (Random.float 0.65 1))
        |> Random.map (List.sortBy Tuple.first)
        |> Random.map
            (List.map
                (\( progress, relativeDistance ) ->
                    followPoint ( cx, cy ) (nodeLocationToPoint progress) relativeDistance
                        |> Tuple.mapFirst ((+) cx)
                        |> Tuple.mapSecond ((+) cy)
                )
            )


random : Float -> Float -> Int -> Random.Generator SvgBlob
random width height nodeCount =
    randomBlobPoints width height nodeCount
        |> Random.map fromPoints


fromPoints : List Point -> SvgBlob
fromPoints points =
    SvgBlob
        { points = points
        , curvedPath = catmullRomClosed 1.0 points |> SubPath.toString
        }


size : SvgBlob -> ( Float, Float )
size (SvgBlob { points }) =
    ( points |> List.map Tuple.first |> List.maximum |> Maybe.withDefault 0
    , points |> List.map Tuple.second |> List.maximum |> Maybe.withDefault 0
    )


view : SvgBlob -> Svg msg
view (SvgBlob { curvedPath }) =
    Svg.path [ Svg.Attributes.d curvedPath ] []
