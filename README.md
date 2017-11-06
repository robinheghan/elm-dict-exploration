# elm-dict-exploration

This is an alternate implementation for `Dict` in Elm. The implementation used is Left-Leaning Red-Black Tree, or LLRB for short. This library is API-compatible, so it should be a drop in replacement. Simply use the following import-statement in your code:

    import Dict.LLRB as Dict exposing (Dict)

You can expect better performance for `insert`, `remove`, `union`, `intersect`, `diff` and any operation implemented with these functions.
