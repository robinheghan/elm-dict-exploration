module Main exposing (main)

import Benchmark.Runner exposing (BenchmarkProgram, program)
import Benchmark exposing (Benchmark, describe, benchmark1, benchmark2, benchmark3, benchmark4)
import Dict as Dict
import Dict.LLRB as Dict2


main : BenchmarkProgram
main =
    program <| suite 100


suite : Int -> Benchmark
suite n =
    let
        half =
            n // 2

        quarter =
            half // 2

        ls =
            List.map3
                (\a b c -> [ a, b, c ])
                (List.map toKeyValuePair (List.range 0 half))
                (List.map toKeyValuePair (List.range quarter (half + quarter) |> List.reverse))
                (List.map toKeyValuePair (List.range (half + 1) n))
                |> List.concat

        toKeyValuePair n =
            ( n, n )

        setLs =
            List.map toKeyValuePair (List.range half (n + half))

        original =
            Dict.fromList ls

        updated =
            Dict2.fromList ls

        originalSetDict =
            Dict.fromList setLs

        updatedSetDict =
            Dict2.fromList setLs

        keys =
            List.map (\( k, v ) -> k) ls
    in
        describe (toString n ++ " elements")
            [ Benchmark.compare "Get"
                (benchmark3 "Original" getter Dict.get keys original)
                (benchmark3 "Updated" getter Dict2.get keys updated)
            , Benchmark.compare "Insert"
                (benchmark1 "Original" Dict.fromList ls)
                (benchmark1 "Updated" Dict2.fromList ls)
            , Benchmark.compare "Remove"
                (benchmark3 "Original" remover Dict.remove keys original)
                (benchmark3 "Updated" remover Dict2.remove keys updated)
            , Benchmark.compare "Remove one item"
                (benchmark3 "Original" singleRemover Dict.remove keys original)
                (benchmark3 "Updated" singleRemover Dict2.remove keys updated)
            , Benchmark.compare "Update insert"
                (benchmark4 "Original" updater Dict.update (\_ -> Just -1) keys original)
                (benchmark4 "Updated" updater Dict2.update (\_ -> Just -1) keys updated)
            , Benchmark.compare "Update remove"
                (benchmark4 "Original" updater Dict.update (\_ -> Nothing) keys original)
                (benchmark4 "Updated" updater Dict2.update (\_ -> Nothing) keys updated)
            , Benchmark.compare "Union"
                (benchmark2 "Original" Dict.union original originalSetDict)
                (benchmark2 "Updated" Dict2.union updated updatedSetDict)
            , Benchmark.compare "Intersect"
                (benchmark2 "Original" Dict.intersect original originalSetDict)
                (benchmark2 "Updated" Dict2.intersect updated updatedSetDict)
            , Benchmark.compare "Diff"
                (benchmark2 "Original" Dict.diff original originalSetDict)
                (benchmark2 "Updated" Dict2.diff updated updatedSetDict)
            ]


getter : (a -> b -> c) -> List a -> b -> List c
getter f keys dict =
    List.foldl (\k acc -> f k dict :: acc) [] keys


updater : (a -> b -> c -> c) -> b -> List a -> c -> c
updater f1 f2 keys dict =
    List.foldl (\k acc -> f1 k f2 acc) dict keys


remover : (a -> b -> b) -> List a -> b -> b
remover f keys dict =
    List.foldl (\k acc -> f k acc) dict keys


singleRemover : (a -> b -> c) -> List a -> b -> List c
singleRemover f keys dict =
    List.foldl (\k acc -> f k dict :: acc) [] keys
