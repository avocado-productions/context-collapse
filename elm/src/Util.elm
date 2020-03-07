module Util exposing (addIndex, findSomething)


addIndex : List a -> List ( Int, a )
addIndex =
    List.indexedMap (\i x -> ( i, x ))


findSomething : (a -> Maybe b) -> List a -> Maybe b
findSomething f items =
    case items of
        [] ->
            Nothing

        x :: xs ->
            case f x of
                Nothing ->
                    findSomething f xs

                Just ans ->
                    Just ans
