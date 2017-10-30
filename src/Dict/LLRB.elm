module Dict.LLRB
    exposing
        ( Dict
        , empty
        , singleton
        , insert
        , update
        , isEmpty
        , get
        , remove
        , member
        , size
        , filter
        , partition
        , foldl
        , foldr
        , map
        , union
        , intersect
        , diff
        , merge
        , keys
        , values
        , toList
        , fromList
        , validateInvariants
        )

{-| A dictionary mapping unique keys to values. The keys can be any comparable
type. This includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or
lists of comparable types.
Insert, remove, and query operations all take *O(log n)* time.


# Dictionaries

@docs Dict


# Build

@docs empty, singleton, insert, remove, update


# Query

@docs isEmpty, size, get, member


# Transform

@docs map, filter, foldl, foldr, partition


# Combine

@docs union, intersect, diff, merge


# Lists

@docs keys, values, toList, fromList

-}

import Basics exposing (..)
import Debug
import Maybe exposing (..)
import List exposing (..)
import String


{-| A dictionary of keys and values. So a `(Dict String User)` is a dictionary
that lets you look up a `String` (such as user names) and find the associated
`User`.
-}
type Dict k v
    = Leaf
      -- isRed key value left right
    | Node Bool k v (Dict k v) (Dict k v)


{-| Create an empty dictionary.
-}
empty : Dict k v
empty =
    Leaf


{-| Determine if a dictionary is empty.
isEmpty empty == True
-}
isEmpty : Dict k v -> Bool
isEmpty dict =
    dict == empty


{-| Create a dictionary with one key-value pair.
-}
singleton : comparable -> v -> Dict comparable v
singleton key value =
    Node False key value Leaf Leaf


{-| Determine the number of key-value pairs in the dictionary.
-}
size : Dict k v -> Int
size dict =
    sizeHelp 0 dict


sizeHelp : Int -> Dict k v -> Int
sizeHelp n dict =
    case dict of
        Leaf ->
            n

        Node _ _ _ left right ->
            sizeHelp (sizeHelp (n + 1) right) left


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]
    get "Tom" animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : comparable -> Dict comparable v -> Maybe v
get targetKey dict =
    case dict of
        Leaf ->
            Nothing

        Node _ key value left right ->
            case compare key targetKey of
                EQ ->
                    Just value

                LT ->
                    get targetKey left

                GT ->
                    get targetKey right


{-| Determine if a key is in a dictionary.
-}
member : comparable -> Dict comparable v -> Bool
member key dict =
    case get key dict of
        Just _ ->
            True

        Nothing ->
            False


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : comparable -> v -> Dict comparable v -> Dict comparable v
insert key value dict =
    case insertHelp key value dict of
        Leaf ->
            Leaf

        Node _ k v left right ->
            Node False k v left right


insertHelp : comparable -> v -> Dict comparable v -> Dict comparable v
insertHelp key value dict =
    case dict of
        Leaf ->
            Node True key value Leaf Leaf

        Node nColor nKey nValue nLeft nRight ->
            case compare key nKey of
                LT ->
                    balance nColor nKey nValue (insertHelp key value nLeft) nRight

                GT ->
                    balance nColor nKey nValue nLeft (insertHelp key value nRight)

                EQ ->
                    Node nColor nKey value nLeft nRight


balance : Bool -> k -> v -> Dict k v -> Dict k v -> Dict k v
balance isRed key value left right =
    case right of
        Node True rK rV rLeft rRight ->
            case left of
                Node True lK lV lLeft lRight ->
                    Node
                        True
                        key
                        value
                        (Node False lK lV lLeft lRight)
                        (Node False rK rV rLeft rRight)

                _ ->
                    Node isRed rK rV (Node True key value left rLeft) rRight

        _ ->
            case left of
                Node True lK lV (Node True llK llV llLeft llRight) lRight ->
                    Node
                        True
                        lK
                        lV
                        (Node False llK llV llLeft llRight)
                        (Node False key value lRight right)

                _ ->
                    Node isRed key value left right


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : comparable -> Dict comparable v -> Dict comparable v
remove targetKey dict =
    case removeHelp targetKey dict of
        Leaf ->
            Leaf

        Node _ k v left right ->
            Node False k v left right


removeHelp : comparable -> Dict comparable v -> Dict comparable v
removeHelp targetKey dict =
    case dict of
        Leaf ->
            Leaf

        Node isRed key value left right ->
            if targetKey < key then
                case left of
                    Node False _ _ lLeft _ ->
                        case lLeft of
                            Node True _ _ _ _ ->
                                Node isRed key value (removeHelp targetKey left) right

                            _ ->
                                case moveRedLeft dict of
                                    Node isRed key value left right ->
                                        balance isRed key value (removeHelp targetKey left) right

                                    Leaf ->
                                        Leaf

                    _ ->
                        Node isRed key value (removeHelp targetKey left) right
            else
                removeHelpEQGT targetKey (removeHelpPrepEQGT targetKey dict isRed key value left right)


removeHelpPrepEQGT : comparable -> Dict comparable v -> Bool -> comparable -> v -> Dict comparable v -> Dict comparable v -> Dict comparable v
removeHelpPrepEQGT targetKey dict isRed key value left right =
    case left of
        Node True lK lV lLeft lRight ->
            Node
                isRed
                lK
                lV
                lLeft
                (Node True key value lRight right)

        _ ->
            case right of
                Node False _ _ (Node False _ _ _ _) _ ->
                    moveRedRight dict

                Node False _ _ Leaf _ ->
                    moveRedRight dict

                _ ->
                    dict


removeHelpEQGT : comparable -> Dict comparable v -> Dict comparable v
removeHelpEQGT targetKey dict =
    case dict of
        Node isRed key value left right ->
            if targetKey == key then
                case getMin right of
                    Node _ minKey minValue _ _ ->
                        balance isRed minKey minValue left (removeMin right)

                    Leaf ->
                        Leaf
            else
                balance isRed key value left (removeHelp targetKey right)

        Leaf ->
            Leaf


getMin : Dict k v -> Dict k v
getMin dict =
    case dict of
        Node _ _ _ ((Node _ _ _ _ _) as left) _ ->
            getMin left

        _ ->
            dict


removeMin : Dict k v -> Dict k v
removeMin dict =
    case dict of
        Node isRed key value ((Node lIsRed _ _ lLeft _) as left) right ->
            if not lIsRed then
                case lLeft of
                    Node True _ _ _ _ ->
                        Node isRed key value (removeMin left) right

                    _ ->
                        case moveRedLeft dict of
                            Node isRed key value left right ->
                                balance isRed key value (removeMin left) right

                            Leaf ->
                                Leaf
            else
                Node isRed key value (removeMin left) right

        _ ->
            Leaf


moveRedLeft : Dict k v -> Dict k v
moveRedLeft dict =
    case dict of
        Node clr k v (Node lClr lK lV lLeft lRight) (Node rClr rK rV ((Node True rlK rlV rlL rlR) as rLeft) rRight) ->
            Node
                True
                rlK
                rlV
                (Node False k v (Node True lK lV lLeft lRight) rlL)
                (Node False rK rV rlR rRight)

        Node clr k v (Node lClr lK lV lLeft lRight) (Node rClr rK rV rLeft rRight) ->
            Node
                (not clr)
                k
                v
                (Node (not lClr) lK lV lLeft lRight)
                (Node (not rClr) rK rV rLeft rRight)

        _ ->
            dict


moveRedRight : Dict k v -> Dict k v
moveRedRight dict =
    case dict of
        Node clr k v (Node lClr lK lV (Node True llK llV llLeft llRight) lRight) (Node rClr rK rV rLeft rRight) ->
            Node
                True
                lK
                lV
                (Node False llK llV llLeft llRight)
                (Node False k v lRight (Node True rK rV rLeft rRight))

        Node clr k v (Node lClr lK lV lLeft lRight) (Node rClr rK rV rLeft rRight) ->
            Node
                (not clr)
                k
                v
                (Node (not lClr) lK lV lLeft lRight)
                (Node (not rClr) rK rV rLeft rRight)

        _ ->
            dict


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : comparable -> (Maybe v -> Maybe v) -> Dict comparable v -> Dict comparable v
update key alter dict =
    case alter (get key dict) of
        Nothing ->
            remove key dict

        Just value ->
            insert key value dict



-- TRANSFORM


{-| Apply a function to all values in a dictionary.
-}
map : (k -> a -> b) -> Dict k a -> Dict k b
map f dict =
    case dict of
        Leaf ->
            Leaf

        Node isRed key value left right ->
            Node isRed key (f key value) (map f left) (map f right)


{-| Keep a key-value pair when it satisfies a predicate.
-}
filter : (comparable -> v -> Bool) -> Dict comparable v -> Dict comparable v
filter predicate dict =
    let
        helper key value acc =
            if predicate key value then
                insert key value acc
            else
                acc
    in
        foldl helper empty dict


{-| Fold over the key-value pairs in a dictionary, in order from lowest
key to highest key.
-}
foldl : (k -> v -> b -> b) -> b -> Dict k v -> b
foldl f acc dict =
    case dict of
        Leaf ->
            acc

        Node _ key value left right ->
            foldl f (f key value (foldl f acc left)) right


{-| Fold over the key-value pairs in a dictionary, in order from highest
key to lowest key.
-}
foldr : (k -> v -> b -> b) -> b -> Dict k v -> b
foldr f acc dict =
    case dict of
        Leaf ->
            acc

        Node _ key value left right ->
            foldr f (f key value (foldr f acc right)) left


{-| Partition a dictionary according to a predicate. The first dictionary
contains all key-value pairs which satisfy the predicate, and the second
contains the rest.
-}
partition : (comparable -> v -> Bool) -> Dict comparable v -> ( Dict comparable v, Dict comparable v )
partition predicate dict =
    let
        helper key value ( trues, falses ) =
            if predicate key value then
                ( insert key value trues, falses )
            else
                ( trues, insert key value falses )
    in
        foldl helper ( empty, empty ) dict



-- COMBINE


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : Dict comparable v -> Dict comparable v -> Dict comparable v
union t1 t2 =
    foldl insert t2 t1


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : Dict comparable v -> Dict comparable v -> Dict comparable v
intersect t1 t2 =
    filter (\k _ -> member k t2) t1


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : Dict comparable v -> Dict comparable v -> Dict comparable v
diff t1 t2 =
    foldl (\k v t -> remove k t) t1 t2


{-| The most general way of combining two dictionaries. You provide three
accumulators for when a given key appears:

1.  Only in the left dictionary.
2.  In both dictionaries.
3.  Only in the right dictionary.
    You then traverse all the keys from lowest to highest, building up whatever
    you want.

-}
merge :
    (comparable -> a -> result -> result)
    -> (comparable -> a -> b -> result -> result)
    -> (comparable -> b -> result -> result)
    -> Dict comparable a
    -> Dict comparable b
    -> result
    -> result
merge leftStep bothStep rightStep leftDict rightDict initialResult =
    let
        stepState rKey rValue ( list, result ) =
            case list of
                [] ->
                    ( list, rightStep rKey rValue result )

                ( lKey, lValue ) :: rest ->
                    if lKey < rKey then
                        stepState rKey rValue ( rest, leftStep lKey lValue result )
                    else if lKey > rKey then
                        ( list, rightStep rKey rValue result )
                    else
                        ( rest, bothStep lKey lValue rValue result )

        ( leftovers, intermediateResult ) =
            foldl stepState ( toList leftDict, initialResult ) rightDict
    in
        List.foldl (\( k, v ) result -> leftStep k v result) intermediateResult leftovers



-- LISTS


{-| Get all of the keys in a dictionary, sorted from lowest to highest.
keys (fromList [(0,"Alice"),(1,"Bob")]) == [0,1]
-}
keys : Dict k v -> List k
keys dict =
    foldr (\key value keyList -> key :: keyList) [] dict


{-| Get all of the values in a dictionary, in the order of their keys.
values (fromList [(0,"Alice"),(1,"Bob")]) == ["Alice", "Bob"]
-}
values : Dict k v -> List v
values dict =
    foldr (\key value valueList -> value :: valueList) [] dict


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : Dict k v -> List ( k, v )
toList dict =
    foldr (\key value list -> ( key, value ) :: list) [] dict


{-| Convert an association list into a dictionary.
-}
fromList : List ( comparable, v ) -> Dict comparable v
fromList assocs =
    List.foldl (\( key, value ) dict -> insert key value dict) empty assocs



{- validation -}


validateInvariants : Dict comparable v -> String
validateInvariants dict =
    if not (isBST dict) then
        "Not in symmetric order"
    else if not (is23 dict) then
        "Not a 2-3 tree"
    else if not (isBalanced dict) then
        "Not balanced"
    else
        ""


isBST : Dict comparable v -> Bool
isBST dict =
    isBSTHelper True (keys dict)


isBSTHelper : Bool -> List comparable -> Bool
isBSTHelper acc keys =
    case keys of
        [] ->
            acc

        x :: [] ->
            acc

        x :: y :: xs ->
            isBSTHelper (acc && x < y) (y :: xs)


is23 : Dict k v -> Bool
is23 dict =
    is23Helper dict dict


is23Helper : Dict k v -> Dict k v -> Bool
is23Helper root node =
    case node of
        Leaf ->
            True

        Node clr _ _ left right ->
            if isRed right then
                False
            else if node /= root && clr && isRed left then
                False
            else
                is23Helper root left && is23Helper root right


isRed : Dict k v -> Bool
isRed dict =
    case dict of
        Node True _ _ _ _ ->
            True

        _ ->
            False


isBalanced : Dict k v -> Bool
isBalanced dict =
    isBalancedHelper dict <| isBalancedBlacksHelper dict 0


isBalancedBlacksHelper : Dict k v -> Int -> Int
isBalancedBlacksHelper node blacks =
    case node of
        Leaf ->
            blacks

        Node isRed _ _ left _ ->
            if isRed then
                isBalancedBlacksHelper left blacks
            else
                isBalancedBlacksHelper left (blacks + 1)


isBalancedHelper : Dict k v -> Int -> Bool
isBalancedHelper node blacks =
    case node of
        Leaf ->
            blacks == 0

        Node isRed _ _ left right ->
            let
                nextBlacks =
                    if isRed then
                        blacks
                    else
                        blacks - 1
            in
                isBalancedHelper left nextBlacks && isBalancedHelper right nextBlacks
