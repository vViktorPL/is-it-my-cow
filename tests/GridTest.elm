module GridTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, intRange, tuple)
import Grid
import Test exposing (..)


fuzzGridSize =
    tuple ( intRange 0 10, intRange 0 10 )


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
                        (Grid.grid2D 3 2)
            , fuzz fuzzGridSize "Created grid is a list with length = cols * rows" <|
                \( cols, rows ) ->
                    Expect.equal (cols * rows)
                        (List.length (Grid.grid2D cols rows))
            ]
        ]
