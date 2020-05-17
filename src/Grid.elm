module Grid exposing (Grid, grid2D, pickRandom2DGridCells)

import Random
import Random.List


type alias Grid =
    List ( Int, Int )


grid2D : Int -> Int -> Grid
grid2D cols rows =
    List.range 0 (rows - 1)
        |> List.concatMap (\y -> List.range 0 (cols - 1) |> List.map (\x -> ( x, y )))


pickRandom2DGridCells : Int -> Grid -> Random.Generator (List ( Int, Int ))
pickRandom2DGridCells n grid =
    Random.List.shuffle grid
        |> Random.map (List.take n)
