module Difficulty exposing (LevelDifficultyParams, forLevelNumber)

import Array exposing (Array)
import List exposing (repeat)
import Timeout exposing (Timeout)


distribution : Array LevelDifficultyParams
distribution =
    [ ( 3, easy )
    , ( 5, medium )
    , ( 10, hard )
    , ( 10, veryHard )
    , ( 1, highestDifficultyLevel )
    ]
        |> List.concatMap
            (\( count, difficulty ) ->
                repeat count difficulty
            )
        |> Array.fromList


highestDifficultyLevel =
    impossible


forLevelNumber : Int -> LevelDifficultyParams
forLevelNumber levelNumber =
    Array.get (levelNumber - 1) distribution
        |> Maybe.withDefault highestDifficultyLevel


easy : LevelDifficultyParams
easy =
    { randomCowsCount = 3
    , mutatedCowsCount = 0
    , mutantSimilarity = 0
    , patchesPerCow = ( 2, 4 )
    , levelTimeout = Timeout.infinite
    , myCowScreenTimeout = Timeout.infinite
    }


medium : LevelDifficultyParams
medium =
    { randomCowsCount = 4
    , mutatedCowsCount = 2
    , mutantSimilarity = 0.3
    , patchesPerCow = ( 4, 5 )
    , levelTimeout = Timeout.infinite
    , myCowScreenTimeout = Timeout.infinite
    }


hard : LevelDifficultyParams
hard =
    { randomCowsCount = 4
    , mutatedCowsCount = 3
    , mutantSimilarity = 0.5
    , patchesPerCow = ( 4, 5 )
    , levelTimeout = Timeout.fromSeconds 20
    , myCowScreenTimeout = Timeout.infinite
    }


veryHard : LevelDifficultyParams
veryHard =
    { randomCowsCount = 2
    , mutatedCowsCount = 5
    , mutantSimilarity = 0.5
    , patchesPerCow = ( 4, 8 )
    , levelTimeout = Timeout.fromSeconds 15
    , myCowScreenTimeout = Timeout.fromSeconds 30
    }


impossible : LevelDifficultyParams
impossible =
    { randomCowsCount = 1
    , mutatedCowsCount = 4
    , mutantSimilarity = 0.7
    , patchesPerCow = ( 4, 8 )
    , levelTimeout = Timeout.fromSeconds 10
    , myCowScreenTimeout = Timeout.fromSeconds 10
    }


type alias LevelDifficultyParams =
    { randomCowsCount : Int
    , mutatedCowsCount : Int
    , mutantSimilarity : Float
    , patchesPerCow : ( Int, Int )
    , levelTimeout : Timeout
    , myCowScreenTimeout : Timeout
    }
