module Main exposing (main)

import Benchmark.Runner exposing (BenchmarkProgram, program)
import Benchmark exposing (Benchmark, describe, benchmark1, benchmark2, benchmark3)
import Dict
import Dict2


main : BenchmarkProgram
main =
    program <| suite 100


suite : Int -> Benchmark
suite n =
    let
        halfwayPoint =
            n // 2

        ls =
            List.indexedMap (,) (List.range 0 n)

        original =
            Dict.fromList ls

        updated =
            Dict2.fromList ls
    in
        describe (toString n ++ " elements")
            [ Benchmark.compare "Singleton"
                (benchmark2 "Original" Dict.singleton n n)
                (benchmark2 "Updated" Dict2.singleton n n)
            , Benchmark.compare "Filter"
                (benchmark2 "Original" Dict.filter (\_ v -> v < halfwayPoint) original)
                (benchmark2 "Updated" Dict2.filter (\_ v -> v < halfwayPoint) updated)
            , Benchmark.compare "Partition"
                (benchmark2 "Original" Dict.partition (\_ v -> v < halfwayPoint) original)
                (benchmark2 "Updated" Dict2.partition (\_ v -> v < halfwayPoint) updated)
            , Benchmark.compare "Insert"
                (benchmark3 "Original" Dict.insert -1 -1 original)
                (benchmark3 "Updated" Dict2.insert -1 -1 updated)
            , Benchmark.compare "Remove"
                (benchmark2 "Original" Dict.remove 0 original)
                (benchmark2 "Updated" Dict2.remove 0 updated)
            ]
