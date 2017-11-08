module Main exposing (main)

import Benchmark.Runner exposing (BenchmarkProgram, program)
import Benchmark exposing (Benchmark, describe, benchmark1, benchmark2, benchmark3, benchmark4)
import Dict
import Dict.LLRB as Dict2


( dictName, dict2Name ) =
    ( "Dict", "LLRB" )


main : BenchmarkProgram
main =
    program <| suite 100


suite : Int -> Benchmark
suite n =
    let
        q =
            n // 4

        ls =
            List.map4
                (\a b c d -> [ a, b, c, d ])
                (List.map (\i -> ( i, i )) (List.range 1 q))
                (List.map (\i -> ( i, i )) (List.range (q + 1) (2 * q) |> List.reverse))
                (List.map (\i -> ( i, i )) (List.range (2 * q + 1) (3 * q)))
                (List.map (\i -> ( i, i )) (List.range (3 * q + 1) n))
                |> List.concat

        setLs =
            List.indexedMap (,) (List.range 1 n)

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
        describe (toString (Dict.size original) ++ " elements")
            [ Benchmark.compare "get"
                (benchmark3 dictName getter Dict.get keys original)
                (benchmark3 dict2Name getter Dict2.get keys updated)
            , Benchmark.compare "fromList"
                (benchmark1 dictName Dict.fromList ls)
                (benchmark1 dict2Name Dict2.fromList ls)
            , Benchmark.compare "insert"
                (benchmark1 dictName (List.foldl (uncurry Dict.insert) Dict.empty) ls)
                (benchmark1 dict2Name (List.foldl (uncurry Dict2.insert) Dict2.empty) ls)
            , Benchmark.compare "remove (until empty)"
                (benchmark3 dictName remover Dict.remove keys original)
                (benchmark3 dict2Name remover Dict2.remove keys updated)
            , Benchmark.compare "remove (one item)"
                (benchmark3 dictName singleRemover Dict.remove keys original)
                (benchmark3 dict2Name singleRemover Dict2.remove keys updated)
            , Benchmark.compare "update insert"
                (benchmark4 dictName updater Dict.update (\_ -> Just -1) keys original)
                (benchmark4 dict2Name updater Dict2.update (\_ -> Just -1) keys updated)
            , Benchmark.compare "update remove"
                (benchmark4 dictName updater Dict.update (\_ -> Nothing) keys original)
                (benchmark4 dict2Name updater Dict2.update (\_ -> Nothing) keys updated)
            , Benchmark.compare "union"
                (benchmark2 dictName Dict.union original originalSetDict)
                (benchmark2 dict2Name Dict2.union updated updatedSetDict)
            , Benchmark.compare "intersect"
                (benchmark2 dictName Dict.intersect original originalSetDict)
                (benchmark2 dict2Name Dict2.intersect updated updatedSetDict)
            , Benchmark.compare "diff"
                (benchmark2 dictName Dict.diff original originalSetDict)
                (benchmark2 dict2Name Dict2.diff updated updatedSetDict)
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
