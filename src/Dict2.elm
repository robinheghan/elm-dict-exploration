module Dict2
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


-- DICTIONARIES
-- BBlack and NBlack should only be used during the deletion
-- algorithm. Any other occurrence is a bug and should fail an assert.


{-| A dictionary of keys and values. So a `(Dict String User)` is a dictionary
that lets you look up a `String` (such as user names) and find the associated
`User`.
-}
type Dict k v
    = Leaf
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
    turnBlack (insertHelp key value dict)


insertHelp : comparable -> v -> Dict comparable v -> Dict comparable v
insertHelp key value dict =
    case dict of
        Leaf ->
            Node True key value Leaf Leaf

        Node isRed nodeKey nodeValue left right ->
            case compare key nodeKey of
                EQ ->
                    Node isRed key value left right

                LT ->
                    balanceLeft isRed nodeKey nodeValue (insertHelp key value left) right

                GT ->
                    balanceRight isRed nodeKey nodeValue left (insertHelp key value right)


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : comparable -> Dict comparable v -> Dict comparable v
remove key dict =
    turnBlack (removeHelp key (turnRed dict))


removeHelp : comparable -> Dict comparable v -> Dict comparable v
removeHelp targetKey dict =
    case dict of
        Leaf ->
            Leaf

        Node isRed key value left right ->
            case compare targetKey key of
                EQ ->
                    removeHelpEQ targetKey isRed key value left right

                LT ->
                    removeHelpLT targetKey isRed key value left right

                GT ->
                    removeHelpGT targetKey isRed key value left right


removeHelpLT : comparable -> Bool -> comparable -> v -> Dict comparable v -> Dict comparable v -> Dict comparable v
removeHelpLT targetKey isRed key value left right =
    if isRed then
        case left of
            Node False _ _ (Node False _ _ _ _) _ ->
                case right of
                    Node False rKey rValue (Node True rlKey rlValue rlLeft rlRight) rRight ->
                        Node
                            True
                            rlKey
                            rlValue
                            (Node False key value (removeHelp targetKey (turnRed left)) rlLeft)
                            (Node False rKey rValue rlRight rRight)

                    _ ->
                        balanceRight
                            False
                            key
                            value
                            (removeHelp targetKey (turnRed left))
                            (turnRed right)

            _ ->
                Node isRed key value (removeHelp targetKey left) right
    else
        Node isRed key value (removeHelp targetKey left) right


removeHelpGT : comparable -> Bool -> comparable -> v -> Dict comparable v -> Dict comparable v -> Dict comparable v
removeHelpGT targetKey isRed key value left right =
    case left of
        Node True lKey lValue lLeft lRight ->
            balanceRight
                True
                lKey
                lValue
                lLeft
                (removeHelp targetKey (Node True key value lRight right))

        _ ->
            if isRed then
                case right of
                    Node False _ _ (Node False _ _ _ _) _ ->
                        case left of
                            Node False lKey lValue ((Node True _ _ _ _) as lLeft) lRight ->
                                Node
                                    True
                                    lKey
                                    lValue
                                    (turnBlack lLeft)
                                    (balanceRight False key value lRight (removeHelp targetKey (turnRed right)))

                            _ ->
                                balanceRight
                                    False
                                    key
                                    value
                                    (turnRed left)
                                    (removeHelp targetKey (turnRed right))

                    _ ->
                        Node True key value left (removeHelp targetKey right)
            else
                Node True key value left (removeHelp targetKey right)


removeHelpEQ : comparable -> Bool -> comparable -> v -> Dict comparable v -> Dict comparable v -> Dict comparable v
removeHelpEQ targetKey isRed key value left right =
    case ( isRed, left, right ) of
        ( True, Leaf, Leaf ) ->
            Leaf

        ( _, Node True lKey lValue lLeft lRight, _ ) ->
            balanceRight
                isRed
                lKey
                lValue
                lLeft
                (removeHelp targetKey (Node True key value lRight right))

        ( True, Node False lKey lValue ((Node True _ _ _ _) as lLeft) lRight, Node False _ _ (Node False _ _ _ _) _ ) ->
            balanceRight
                True
                lKey
                lValue
                (turnBlack lLeft)
                (case getMin right of
                    Node _ mKey mValue _ _ ->
                        balanceRight False mKey mValue lRight (deleteMin (turnRed right))

                    Leaf ->
                        Leaf
                )

        ( True, _, Node False _ _ (Node False _ _ _ _) _ ) ->
            case getMin right of
                Node _ mKey mValue _ _ ->
                    balanceRight
                        False
                        mKey
                        mValue
                        (turnRed left)
                        (deleteMin (turnRed right))

                Leaf ->
                    Leaf

        ( True, _, Node False _ _ _ _ ) ->
            case getMin right of
                Node _ mKey mValue _ _ ->
                    Node
                        True
                        mKey
                        mValue
                        left
                        (deleteMin right)

                Leaf ->
                    Leaf

        _ ->
            Leaf


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : comparable -> (Maybe v -> Maybe v) -> Dict comparable v -> Dict comparable v
update key alter dict =
    case alter (get key dict) of
        Nothing ->
            remove key dict

        Just value ->
            insert key value dict



-- HELPERS


balanceLeft : Bool -> k -> v -> Dict k v -> Dict k v -> Dict k v
balanceLeft isRed key value left right =
    case ( isRed, left ) of
        ( False, Node True lKey lValue ((Node True _ _ _ _) as lLeft) lRight ) ->
            Node
                True
                lKey
                lValue
                (turnBlack lLeft)
                (Node False key value lRight right)

        _ ->
            Node isRed key value left right


balanceRight : Bool -> k -> v -> Dict k v -> Dict k v -> Dict k v
balanceRight isRed key value left right =
    case right of
        Node True rKey rValue rLeft rRight ->
            case left of
                Node True _ _ _ _ ->
                    Node True key value (turnBlack left) (turnBlack right)

                _ ->
                    Node isRed rKey rValue (Node True key value left rLeft) rRight

        _ ->
            Node isRed key value left right


turnBlack : Dict k v -> Dict k v
turnBlack dict =
    case dict of
        Node _ key value left right ->
            Node False key value left right

        Leaf ->
            Leaf


turnRed : Dict k v -> Dict k v
turnRed dict =
    case dict of
        Node _ key value left right ->
            Node True key value left right

        Leaf ->
            Leaf


moveRedLeft : Dict k v -> Dict k v
moveRedLeft dict =
    let
        flipped =
            colorFlip dict
    in
        case flipped of
            Node isRed key value left ((Node _ _ _ (Node True _ _ _ _) _) as right) ->
                Node isRed key value left (rotateRight right)
                    |> rotateLeft
                    |> colorFlip

            _ ->
                flipped


moveRedRight : Dict k v -> Dict k v
moveRedRight dict =
    let
        flipped =
            colorFlip dict
    in
        case flipped of
            Node isRed key value ((Node _ _ _ (Node True _ _ _ _) _) as left) right ->
                flipped
                    |> rotateRight
                    |> colorFlip

            _ ->
                flipped


rotateLeft : Dict k v -> Dict k v
rotateLeft dict =
    case dict of
        Node isRed key value left (Node rRed rKey rValue rLeft rRight) ->
            Node
                isRed
                rKey
                rValue
                (Node True key value left rLeft)
                rRight

        _ ->
            dict


rotateRight : Dict k v -> Dict k v
rotateRight dict =
    case dict of
        Node isRed key value (Node lRed lKey lValue lLeft lRight) right ->
            Node
                isRed
                lKey
                lValue
                lLeft
                (Node True key value lRight right)

        _ ->
            dict


colorFlip : Dict k v -> Dict k v
colorFlip dict =
    case dict of
        Node isRed key value (Node lRed lKey lValue lLeft lRight) (Node rRed rKey rValue rLeft rRight) ->
            Node
                (not isRed)
                key
                value
                (Node (not lRed) lKey lValue lLeft lRight)
                (Node (not rRed) rKey rValue rLeft rRight)

        _ ->
            dict


deleteMin : Dict k v -> Dict k v
deleteMin dict =
    case dict of
        Node isRed key value left right ->
            case left of
                Node False _ _ (Node False _ _ _ _) _ ->
                    case moveRedLeft dict of
                        Node isRed key value left right ->
                            balanceLeft isRed key value (deleteMin left) right

                        Leaf ->
                            Leaf

                Leaf ->
                    Leaf

                _ ->
                    balanceLeft isRed key value (deleteMin left) right

        Leaf ->
            Leaf


getMin : Dict k v -> Dict k v
getMin dict =
    case dict of
        Node _ _ _ ((Node _ _ _ _ _) as left) _ ->
            getMin left

        _ ->
            dict



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
