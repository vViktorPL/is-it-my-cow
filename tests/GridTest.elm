module GridTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, intRange, tuple)
import Grid
import List.Nonempty
import Test exposing (..)


fuzzGridSize =
    tuple ( intRange 1 10, intRange 1 10 )


suite : Test
suite =
    describe "Grid module"
        [ describe "2D grid"
            [ test "Creates valid 3x2 grid" <|
                \_ ->
                    Expect.equal
                        [ ( 0, 0 )
                        , ( 1, 0 )
                        , ( 2, 0 )
                        , ( 0, 1 )
                        , ( 1, 1 )
                        , ( 2, 1 )
                        ]
                        (List.Nonempty.toList <| Grid.grid2D 3 2)
            , test "Creates 1 element grid for 0x0 size" <|
                \_ ->
                    Expect.equal
                        ( 0, 0 )
                        (List.Nonempty.head <| Grid.grid2D 0 0)
            , fuzz fuzzGridSize "Created grid is a list with length = cols * rows" <|
                \( cols, rows ) ->
                    Expect.equal (cols * rows)
                        (List.Nonempty.length (Grid.grid2D cols rows))
            ]
        ]
