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
    describe (toString n ++ " elements")
        [ suiteBuild n
        , suiteQuery n
        , suiteModify n
        , suiteCombineContiguous n
        , suiteCombineFragmented n
        , suiteTransform n
        ]


suiteBuild : Int -> Benchmark
suiteBuild n =
    let
        -- mix up list
        q =
            n // 4

        assocList =
            List.map4
                (\a b c d -> [ a, b, c, d ])
                (assocListRange 1 q)
                (assocListRange (q + 1) (2 * q) |> List.reverse)
                (assocListRange (2 * q + 1) (3 * q))
                (assocListRange (3 * q + 1) n |> List.reverse)
                |> List.concat
    in
        describe "Build"
            [ Benchmark.compare "fromList"
                (benchmark1 dictName Dict.fromList assocList)
                (benchmark1 dict2Name Dict2.fromList assocList)
            , Benchmark.compare "insert (from empty)"
                (benchmark3 dictName List.foldl (uncurry Dict.insert) Dict.empty assocList)
                (benchmark3 dict2Name List.foldl (uncurry Dict2.insert) Dict2.empty assocList)
            ]


suiteQuery : Int -> Benchmark
suiteQuery n =
    let
        assocList =
            assocListRange 1 n

        keys =
            assocList |> List.map Tuple.first

        ( dict, dict2 ) =
            ( Dict.fromList assocList, Dict2.fromList assocList )
    in
        describe "Query"
            [ Benchmark.compare "get"
                (benchmark2 dictName List.map ((flip Dict.get) dict) keys)
                (benchmark2 dict2Name List.map ((flip Dict2.get) dict2) keys)
            ]


suiteModify : Int -> Benchmark
suiteModify n =
    let
        -- make dicts from evens; use odds to double their size
        ( evens, odds ) =
            assocListRange 1 (2 * n) |> List.partition (\( k, _ ) -> k % 2 == 0)

        ( keys, oddKeys ) =
            ( evens |> List.map Tuple.first, odds |> List.map Tuple.first )

        ( dict, dict2 ) =
            ( Dict.fromList evens, Dict2.fromList evens )
    in
        describe "Modify"
            [ Benchmark.compare "insert (doubling size)"
                (benchmark3 dictName List.foldl (uncurry Dict.insert) dict odds)
                (benchmark3 dict2Name List.foldl (uncurry Dict2.insert) dict2 odds)
            , Benchmark.compare "remove (until empty)"
                (benchmark3 dictName List.foldl Dict.remove dict keys)
                (benchmark3 dict2Name List.foldl Dict2.remove dict2 keys)
            , Benchmark.compare "remove (only one)"
                (benchmark2 dictName List.map ((flip Dict.remove) dict) keys)
                (benchmark2 dict2Name List.map ((flip Dict2.remove) dict2) keys)
            , Benchmark.compare "update (modify all)"
                (benchmark3 dictName List.foldl ((flip Dict.update) (\_ -> Just 0)) dict keys)
                (benchmark3 dict2Name List.foldl ((flip Dict2.update) (\_ -> Just 0)) dict2 keys)
            , Benchmark.compare "update (remove all)"
                (benchmark3 dictName List.foldl ((flip Dict.update) (\_ -> Nothing)) dict keys)
                (benchmark3 dict2Name List.foldl ((flip Dict2.update) (\_ -> Nothing)) dict2 keys)
            , Benchmark.compare "update (insert, doubling size)"
                (benchmark3 dictName List.foldl ((flip Dict.update) (\_ -> Just 0)) dict oddKeys)
                (benchmark3 dict2Name List.foldl ((flip Dict2.update) (\_ -> Just 0)) dict2 oddKeys)
            ]


suiteCombineContiguous : Int -> Benchmark
suiteCombineContiguous n =
    let
        ( list, shiftedList ) =
            ( assocListRange 1 n, assocListRange (n // 2 + 1) (n // 2 + n) )

        ( left, right ) =
            ( Dict.fromList list, Dict.fromList shiftedList )

        ( left2, right2 ) =
            ( Dict2.fromList list, Dict2.fromList shiftedList )
    in
        describe "Combine (contiguous intersection)"
            [ Benchmark.compare "union"
                (benchmark2 dictName Dict.union left right)
                (benchmark2 dict2Name Dict2.union left2 right2)
            , Benchmark.compare "intersect"
                (benchmark2 dictName Dict.intersect left right)
                (benchmark2 dict2Name Dict2.intersect left2 right2)
            , Benchmark.compare "diff"
                (benchmark2 dictName Dict.diff left right)
                (benchmark2 dict2Name Dict2.diff left2 right2)
            ]


suiteCombineFragmented : Int -> Benchmark
suiteCombineFragmented n =
    let
        ( multiples2, multiples3 ) =
            ( assocListRange 1 (2 * n) |> List.filter (\( k, _ ) -> k % 2 == 0)
            , assocListRange 1 (3 * n) |> List.filter (\( k, _ ) -> k % 3 == 0)
            )

        ( left, right ) =
            ( Dict.fromList multiples2, Dict.fromList multiples3 )

        ( left2, right2 ) =
            ( Dict2.fromList multiples2, Dict2.fromList multiples3 )
    in
        describe "Combine (fragmented intersection)"
            [ Benchmark.compare "union"
                (benchmark2 dictName Dict.union left right)
                (benchmark2 dict2Name Dict2.union left2 right2)
            , Benchmark.compare "intersect"
                (benchmark2 dictName Dict.intersect left right)
                (benchmark2 dict2Name Dict2.intersect left2 right2)
            , Benchmark.compare "diff"
                (benchmark2 dictName Dict.diff left right)
                (benchmark2 dict2Name Dict2.diff left2 right2)
            ]


suiteTransform : Int -> Benchmark
suiteTransform n =
    let
        assocList =
            assocListRange 1 n

        ( dict, dict2 ) =
            ( Dict.fromList assocList, Dict2.fromList assocList )
    in
        describe "Transform"
            [ Benchmark.compare "filter"
                (benchmark2 dictName Dict.filter (\k _ -> k % 2 == 0) dict)
                (benchmark2 dict2Name Dict2.filter (\k _ -> k % 2 == 0) dict2)
            , Benchmark.compare "partition"
                (benchmark2 dictName Dict.partition (\k _ -> k % 2 == 0) dict)
                (benchmark2 dict2Name Dict2.partition (\k _ -> k % 2 == 0) dict2)
            ]


assocListRange : Int -> Int -> List ( Int, Int )
assocListRange start end =
    List.range start end |> List.map (\x -> ( x, x ))
