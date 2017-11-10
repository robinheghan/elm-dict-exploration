module Main exposing (main)

import Benchmark.Runner exposing (BenchmarkProgram, program)
import Benchmark exposing (Benchmark, describe, benchmark, benchmark1, benchmark2, benchmark3, benchmark4)
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
        , suiteCombineFragmented n
        , suiteCombineContiguous n
        , suiteCombineDisjoint n
        , suiteCombineEmpty n
        , suiteTransform n
        ]


suiteBuild : Int -> Benchmark
suiteBuild n =
    let
        -- build from an unsorted list
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


suiteCombineFragmented : Int -> Benchmark
suiteCombineFragmented n =
    suiteCombine "Combine (fragmented intersections)"
        -- multiples of 2
        (assocListRange 1 (2 * n) |> List.filter (\( k, _ ) -> k % 2 == 0))
        -- multiples of 3
        (assocListRange 1 (3 * n) |> List.filter (\( k, _ ) -> k % 3 == 0))


suiteCombineContiguous : Int -> Benchmark
suiteCombineContiguous n =
    suiteCombine "Combine (contiguous intersection)"
        (assocListRange 1 n)
        (assocListRange (n // 2 + 1) (n // 2 + n))


suiteCombineDisjoint : Int -> Benchmark
suiteCombineDisjoint n =
    suiteCombine "Combine (disjoint ranges)"
        (assocListRange 1 n)
        (assocListRange (n + 1) (n + n))


suiteCombineEmpty : Int -> Benchmark
suiteCombineEmpty n =
    suiteCombine "Combine (with one empty)"
        (assocListRange 1 n)
        []


suiteCombine : String -> List ( comparable, v ) -> List ( comparable, v ) -> Benchmark
suiteCombine description leftList rightList =
    let
        ( left, right ) =
            ( Dict.fromList leftList, Dict.fromList rightList )

        ( left2, right2 ) =
            ( Dict2.fromList leftList, Dict2.fromList rightList )
    in
        describe description
            [ Benchmark.compare "union"
                (benchmark2Commutative dictName Dict.union left right)
                (benchmark2Commutative dict2Name Dict2.union left2 right2)
            , Benchmark.compare "intersect"
                (benchmark2Commutative dictName Dict.intersect left right)
                (benchmark2Commutative dict2Name Dict2.intersect left2 right2)
            , Benchmark.compare "diff"
                (benchmark2Commutative dictName Dict.diff left right)
                (benchmark2Commutative dict2Name Dict2.diff left2 right2)
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


benchmark2Commutative : String -> (a -> a -> b) -> a -> a -> Benchmark
benchmark2Commutative name op left right =
    benchmark name (\() -> ( op left right, op right left ))
