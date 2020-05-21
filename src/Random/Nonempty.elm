module Random.Nonempty exposing (choose, nonempty, shuffle)

import List.Nonempty exposing (Nonempty)
import Random
import Random.List


nonempty : Int -> Random.Generator a -> Random.Generator (Nonempty a)
nonempty howMuchRestElements entryGenerator =
    Random.pair
        entryGenerator
        (Random.list howMuchRestElements entryGenerator)
        |> Random.map (\( first, rest ) -> List.Nonempty.Nonempty first rest)


shuffle : Nonempty a -> Random.Generator (Nonempty a)
shuffle l =
    l
        |> List.Nonempty.toList
        |> Random.List.shuffle
        |> Random.map
            (\shuffled ->
                case shuffled of
                    first :: rest ->
                        List.Nonempty.Nonempty first rest

                    [] ->
                        List.Nonempty.Nonempty (List.Nonempty.head l) []
            )


choose : Nonempty a -> Random.Generator a
choose l =
    l
        |> List.Nonempty.toList
        |> Random.List.choose
        |> Random.map
            (\( maybeElement, _ ) ->
                Maybe.withDefault (List.Nonempty.head l) maybeElement
            )
