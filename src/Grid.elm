module Grid exposing (Grid, grid2D, pickRandom2DGridCells)

import List.Nonempty exposing (Nonempty)
import Random
import Random.Nonempty


type alias Grid =
    Nonempty ( Int, Int )


grid2D : Int -> Int -> Grid
grid2D cols rows =
    let
        elements =
            List.range 0 (rows - 1)
                |> List.concatMap (\y -> List.range 0 (cols - 1) |> List.map (\x -> ( x, y )))
    in
    case elements of
        first :: rest ->
            List.Nonempty.Nonempty first rest

        [] ->
            List.Nonempty.Nonempty ( 0, 0 ) []


pickRandom2DGridCells : Int -> Grid -> Random.Generator (Nonempty ( Int, Int ))
pickRandom2DGridCells n grid =
    Random.Nonempty.shuffle grid
        |> Random.map (\(List.Nonempty.Nonempty head tail) -> List.Nonempty.Nonempty head (List.take (n - 1) tail))
