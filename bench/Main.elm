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

        chosenIndex =
            -1
    in
        describe (toString n ++ " elements")
            [ Benchmark.compare "Get"
                (benchmark2 "Original" Dict.get chosenIndex original)
                (benchmark2 "Updated" Dict2.get chosenIndex updated)
            , Benchmark.compare "Insert"
                (benchmark3 "Original" Dict.insert chosenIndex -1 original)
                (benchmark3 "Updated" Dict2.insert chosenIndex -1 updated)
            , Benchmark.compare "Update insert"
                (benchmark3 "Original" Dict.update chosenIndex (always <| Just -1) original)
                (benchmark3 "Updated" Dict2.update chosenIndex (always <| Just -1) updated)
            , Benchmark.compare "Remove"
                (benchmark2 "Original" Dict.remove chosenIndex original)
                (benchmark2 "Updated" Dict2.remove chosenIndex updated)
            , Benchmark.compare "Update remove"
                (benchmark3 "Original" Dict.update chosenIndex (always Nothing) original)
                (benchmark3 "Updated" Dict2.update chosenIndex (always Nothing) updated)
            ]
