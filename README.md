# elm-dict-exploration

This is an alternate implementation for `Dict` in Elm. The implementation used is Left-Leaning Red-Black Tree. or LLRB for short. This library is API-compatible, so it should be a drop in replacement. Simply use the following impor-statement in your code:

    import Dict.LLRB as Dict exposing (Dict)

You can expect better performance for `get` (due to better balancing), `insert` due to simpler balancing and `remove` (again, simpler balancing). This implementation is also shorter, so you should see a minor code reduction as well.