module Storage exposing (Storage, addString, empty, emptyAbort, emptyLog, getInt, getMaybeInt, getMaybeString, getString, getStrings, setInt, setString)

import Dict exposing (Dict)


type Value
    = Ints (List Int)
    | Int Int
    | String String
    | Strings (List String)


type Storage
    = Storage
        { dict : Dict String Value
        , log : String -> String -> ()
        }


empty : Storage
empty =
    Storage { dict = Dict.empty, log = \_ _ -> () }


emptyLog : (String -> String -> String) -> Storage
emptyLog log =
    Storage
        { dict = Dict.empty
        , log = \x y -> (\_ -> ()) (log x y)
        }


emptyAbort : (String -> ()) -> Storage
emptyAbort abort =
    Storage
        { dict = Dict.empty
        , log = \x y -> abort ("error in " ++ x ++ ": " ++ y)
        }


toString : Value -> String
toString value =
    case value of
        Ints xs ->
            "integers {" ++ (List.map String.fromInt xs |> List.intersperse "," |> String.concat) ++ "}"

        Int x ->
            "integer " ++ String.fromInt x

        Strings xs ->
            "strings {" ++ (xs |> List.intersperse "," |> String.concat) ++ "}"

        String x ->
            "string " ++ x


toStringBrief : Value -> String
toStringBrief value =
    case value of
        Ints _ ->
            "integers"

        Int _ ->
            "integer"

        Strings _ ->
            "strings"

        String _ ->
            "string "


getInt : String -> Storage -> Int
getInt key (Storage store) =
    case Dict.get key store.dict of
        Nothing ->
            let
                () =
                    store.log "getInt" <| "no value with key " ++ key
            in
            -1234

        Just (Int x) ->
            x

        Just value ->
            let
                () =
                    store.log "getInt" <| "expected an integer value for key " ++ key ++ ", but found " ++ toString value
            in
            -15312


getMaybeInt : String -> Storage -> Maybe Int
getMaybeInt key (Storage store) =
    Dict.get key store.dict
        |> Maybe.map
            (\value ->
                case value of
                    Int x ->
                        x

                    _ ->
                        let
                            () =
                                store.log "getMaybeInt" <| "expected an integer value for key " ++ key ++ ", but found " ++ toString value
                        in
                        84
            )


setInt : String -> Int -> Storage -> Storage
setInt key value (Storage store) =
    Storage
        { store
            | dict =
                Dict.update key
                    (\prev ->
                        let
                            () =
                                case prev of
                                    Nothing ->
                                        ()

                                    Just (Int _) ->
                                        ()

                                    Just old ->
                                        store.log "setInt" <| "replacing " ++ toString old ++ " with " ++ toString (Int value)
                        in
                        Just (Int value)
                    )
                    store.dict
        }


getString : String -> Storage -> String
getString key (Storage store) =
    case Dict.get key store.dict of
        Nothing ->
            let
                () =
                    store.log "getString" <| "no value with key " ++ key
            in
            "__no_value_for_key_" ++ key

        Just (String x) ->
            x

        Just value ->
            let
                () =
                    store.log "getString" <| "expected a string value for key " ++ key ++ ", but found " ++ toString value
            in
            "__wrong_value_for_key_" ++ key


getMaybeString : String -> Storage -> Maybe String
getMaybeString key (Storage store) =
    Dict.get key store.dict
        |> Maybe.map
            (\value ->
                case value of
                    String x ->
                        x

                    _ ->
                        let
                            () =
                                store.log "getMaybeString" <| "expected an string value for key " ++ key ++ ", but found " ++ toString value
                        in
                        "__wrong_value_for_key_" ++ key
            )


setString : String -> String -> Storage -> Storage
setString key value (Storage store) =
    Storage
        { store
            | dict =
                Dict.update key
                    (\prev ->
                        let
                            () =
                                case prev of
                                    Nothing ->
                                        ()

                                    Just (String _) ->
                                        ()

                                    Just old ->
                                        store.log "setString" <| "replacing " ++ toString old ++ " with " ++ toString (String value)
                        in
                        Just (String value)
                    )
                    store.dict
        }


getStrings : String -> Storage -> List String
getStrings key (Storage store) =
    case Dict.get key store.dict of
        Nothing ->
            []

        Just (Strings xs) ->
            xs

        Just value ->
            let
                () =
                    store.log "getStrings" <| "expected string values for key " ++ key ++ ", but found " ++ toString value
            in
            [ "__wrong_value_for_key_" ++ key ]


addString : String -> String -> Storage -> Storage
addString key value (Storage store) =
    Storage
        { store
            | dict =
                Dict.update key
                    (\prev ->
                        let
                            existing =
                                case prev of
                                    Nothing ->
                                        []

                                    Just (Strings xs) ->
                                        xs

                                    Just old ->
                                        (\() -> []) <| store.log "addString" <| "replacing " ++ toString old ++ " with " ++ toString (String value)
                        in
                        Just (Strings (value :: existing))
                    )
                    store.dict
        }
