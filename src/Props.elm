module Props exposing
    ( Props, Key, emptyLog, emptyAbort
    , expectFlag, getFlag, setFlag
    , expectMaybeBool, getMaybeBool, setMaybeBool
    , expectInt, getInt, setInt
    , expectMaybeInt, getMaybeInt, setMaybeInt
    , expectString, getString, setString
    , expectStrings, getStrings, addString, setStrings
    , expectMaybeString, getMaybeString, setMaybeString
    , emptySilent
    )

{-| Flexible but persnickety dynamically typed key-value maps.

If you want to have key-value properties where you are responsible
for correctly keeping track of the types of every key, then you should
probably think very, very carefully about what you're doing... but if
you're sure, maybe this is for you.


## Example

You create a `Props` map by starting from an empty map and
setting keys to their associated values.

     nest : Props
     nest =
        Props.empty
            |> Props.setString "name" "Gertrude's Hut"
            |> Props.setInt "eggs" 4

At this point, `nest` is basically a dynamically-typed record akin
to the Elm record `{ name = "Gertrude's Hut", eggs = 4 }`.

     nest |> Props.getInt "eggs"     -- == 4
     nest |> Props.getString "name"  -- == "Gertrude's Hut"

     nest |> Props.setInt "eggs" 3
        |> Props.getInt "eggs"       -- == 3

     nest |> Props.setFlag "cozy" True
        |> Props.getFlag "cozy"      -- == True

     nest |> Props.getFlag "eggs"    -- XXX OH NO ERROR UNDEFINED
     nest |> Props.getInt "name"     -- XXX OH NO ERROR UNDEFINED
     nest |> Props.getString "cozy"  -- XXX OH NO ERROR UNDEFINED

As the last three lines detail, using this library is playing with fire.


## Don't screw this up

But once a key has been associated with a particular type, that
prop forever associates that type with that key.

     veryBad : Props
     veryBad =
        Props.empty
            |> Props.setInt "field" 12
            |> Props.setFlag "field" True

The `Props` object `veryBad` is _irrevocably broken_. It will not remember
anything that is set, it will not return anything of use. The library will
_not_ cause your browser to enter an infinite loop and lock the UI, but
within the constraints of the Elm type system it aims to be as broken
as possible as soon as anything has gone wrong.


## At least tell me what happened

The `emptyLog` and `emptyAbort` functions are designed to make it easier
to work with the inevitable errors you will encounter from using this
ill-advised library.

By passing `emptyLog` or `emptyAbort` the `Debug.log` or `Debug.todo`
functions, respectively, you will cause an error message to be generated
at the first point where the `Props` object gets accessed to read or
write at the wrong type.

     veryBadLog : Props
     veryBadLog =
        Props.emptyLog { log = Debug.log }
            |> Props.setInt "field" 12     -- this is fine
            |> Props.setInt "field" 5      -- still okay
            |> Props.setFlag "field" True  -- log message generated here
            |> Props.setFlag "field" False -- this is ignored, already broken

     veryBadAbort : Props
     veryBadAbort =
        Props.emptyAbort { todo = Debug.todo }
            |> Props.setInt "field" 12     -- this is fine
            |> Props.setInt "field" 55     -- still okay
            |> Props.setFlag "field" True  -- program crashes here
            |> Props.setFlag "field" False -- never gets here


# Creating `Props`

@docs Props, Key, empty, emptyLog, emptyAbort


# Flags

Boolean properties are called flags. Keys associated with the type
`Bool` cannot be used with getters and setters for any other type, including
`Maybe Bool`.

@docs expectFlag, getFlag, setFlag


## Optional Booleans

Keys associated with the type `Maybe Bool` cannot be used with getters or
setters for any other type, including `Bool`.

@docs expectMaybeBool, getMaybeBool, setMaybeBool


# Integers

Keys associated with the type `Int` cannot be used with getters or
setters for any other type, including `Maybe Int`.

@docs expectInt, getInt, setInt


## Optional Integers

Keys associated with the type `Maybe Int` cannot be used with getters or
setters for any other type, including `Int`.

@docs expectMaybeInt, getMaybeInt, setMaybeInt


# Strings

Keys associated with the type `String` cannot be used with getters or
setters for any other type, including `List String` and `Maybe String`.

@docs expectString, getString, setString


## Lists of Strings

Keys associated with the type `List String` cannot be used with getters or
setters for any other type, including `String` and `Maybe String`.

@docs expectStrings, getStrings, addString, setStrings


## Optional Strings

Keys associated with the type `Maybe String` cannot be used with getters or
setters for any other type, including `String` and `List String`.

@docs expectMaybeString, getMaybeString, setMaybeString

-}

import Dict exposing (Dict)


type Initializable a
    = NotInitialized
    | Initialized a


type Value
    = Bool (Initializable Bool)
    | MaybeBool (Initializable (Maybe Bool))
    | Int (Initializable Int)
    | Ints (Initializable (List Int))
    | MaybeInt (Initializable (Maybe Int))
    | String (Initializable String)
    | Strings (Initializable (List String))
    | MaybeString (Initializable (Maybe String))


explain : Value -> String
explain value =
    if initialized value then
        "contained " ++ toStringFull value

    else
        "expected to store values of type " ++ toStringBrief value


toStringFull : Value -> String
toStringFull value =
    case value of
        Bool NotInitialized ->
            "an uninitialized Bool (Flag)"

        Bool (Initialized True) ->
            "a set flag (the boolean value True)"

        Bool (Initialized False) ->
            "an unset flag (the boolean value False)"

        MaybeBool NotInitialized ->
            "an uninitialized Maybe Bool"

        MaybeBool (Initialized Nothing) ->
            "Nothing (with type Maybe Bool)"

        MaybeBool (Initialized (Just True)) ->
            "Just True"

        MaybeBool (Initialized (Just False)) ->
            "Just False"

        Int NotInitialized ->
            "an uninitialized Int"

        Int (Initialized n) ->
            "the Int " ++ String.fromInt n

        Ints NotInitialized ->
            "an uninitialized list of Int"

        Ints (Initialized ns) ->
            "the list of Int [ " ++ (List.map String.fromInt ns |> List.intersperse ", " |> String.concat) ++ " ]"

        MaybeInt NotInitialized ->
            "an uninitialized Maybe Int"

        MaybeInt (Initialized Nothing) ->
            "Nothing (with type Maybe Int)"

        MaybeInt (Initialized (Just n)) ->
            "Just " ++ String.fromInt n

        String NotInitialized ->
            "an uninitialized String"

        String (Initialized str) ->
            -- TODO re-escape these?
            "the String \"" ++ str ++ "\""

        Strings NotInitialized ->
            "an uninitialized list of Int"

        Strings (Initialized strs) ->
            "the list of String [ " ++ (List.map (\str -> "\"" ++ str ++ "\"") strs |> List.intersperse ", " |> String.concat) ++ " ]"

        MaybeString NotInitialized ->
            "an uninitialized Maybe String"

        MaybeString (Initialized Nothing) ->
            "Nothing (with type Maybe String)"

        MaybeString (Initialized (Just str)) ->
            "Just \"" ++ str ++ "\""


initialized : Value -> Bool
initialized value =
    case value of
        Bool init ->
            init /= NotInitialized

        MaybeBool init ->
            init /= NotInitialized

        Int init ->
            init /= NotInitialized

        Ints init ->
            init /= NotInitialized

        MaybeInt init ->
            init /= NotInitialized

        String init ->
            init /= NotInitialized

        Strings init ->
            init /= NotInitialized

        MaybeString init ->
            init /= NotInitialized


typesMatch : Value -> Value -> Bool
typesMatch v1 v2 =
    case v1 of
        Bool _ ->
            case v2 of
                Bool _ ->
                    True

                _ ->
                    False

        MaybeBool _ ->
            case v2 of
                MaybeBool _ ->
                    True

                _ ->
                    False

        Int _ ->
            case v2 of
                Int _ ->
                    True

                _ ->
                    False

        Ints _ ->
            case v2 of
                Ints _ ->
                    True

                _ ->
                    False

        MaybeInt _ ->
            case v2 of
                MaybeInt _ ->
                    True

                _ ->
                    False

        String _ ->
            case v2 of
                String _ ->
                    True

                _ ->
                    False

        Strings _ ->
            case v2 of
                Strings _ ->
                    True

                _ ->
                    False

        MaybeString _ ->
            case v2 of
                MaybeString _ ->
                    True

                _ ->
                    False


toStringBrief : Value -> String
toStringBrief value =
    case value of
        Bool _ ->
            "Bool (Flag)"

        MaybeBool _ ->
            "Maybe Bool"

        Int _ ->
            "Int"

        Ints _ ->
            "List Int"

        MaybeInt _ ->
            "MaybeInt"

        String _ ->
            "Int"

        Strings _ ->
            "List String"

        MaybeString _ ->
            "Maybe String"


reportInvalidExpecation : String -> String -> Value -> Value -> Props -> ()
reportInvalidExpecation function key stored expected (Props props) =
    props.log function <|
        "I was instructed to force the key "
            ++ key
            ++ " to have type "
            ++ toStringBrief expected
            ++ ".\n"
            ++ "However, the Props object already "
            ++ explain stored
            ++ " for that key."


reportInvalidGet : String -> String -> Maybe Value -> Value -> Props -> ()
reportInvalidGet function key stored expected (Props props) =
    props.log function <|
        case stored of
            Nothing ->
                "I needed to get the "
                    ++ toStringBrief expected
                    ++ " associated with the key "
                    ++ key
                    ++ " but there was no value associated with that key."

            Just storedValue ->
                if typesMatch storedValue expected && not (initialized storedValue) then
                    "I needed to get the "
                        ++ toStringBrief expected
                        ++ " associated with the key "
                        ++ key
                        ++ " but there was no value associated with that key.\n"
                        ++ "(It's not enough to expect a "
                        ++ toStringBrief expected
                        ++ ", a definite value must be actually set.)"

                else
                    -- Must be a type mismatch
                    "I needed to get the "
                        ++ toStringBrief expected
                        ++ " associated with the key "
                        ++ key
                        ++ ".\nHowever, the Props object already "
                        ++ explain storedValue
                        ++ "."


reportInvalidSet : String -> String -> Value -> Value -> Props -> ()
reportInvalidSet function key stored added (Props props) =
    props.log function <|
        "I was told to associate the key "
            ++ key
            ++ " with "
            ++ toStringFull added
            ++ ".\nThe object already "
            ++ explain stored
            ++ " with this key.\nAssociating a key with two different types irrevocably corrupts the object."


{-| A `Props` object is a map from keys (strings) to values.
-}
type Props
    = Props
        { dict : Dict String Value
        , log : String -> String -> ()
        , broken : Bool
        }


{-| The props object is accessed with keys (which are just strings).
The key is always the first argument.
-}
type alias Key =
    String


{-| An empty `Props` object. All `Props` that derive from this
empty object will silently return garbage if they are corrupted.
-}
emptySilent : Props
emptySilent =
    Props { dict = Dict.empty, log = \_ _ -> (), broken = False }


{-| An empty `Props` object that will log errors before returning garbage if passed the function `Debug.log` as an argument.
-}
emptyLog : { log : String -> String -> String } -> Props
emptyLog { log } =
    Props
        { dict = Dict.empty
        , log = \x y -> (\_ -> ()) (log ("In function " ++ x) y)
        , broken = False
        }


butFirst : a -> b -> b
butFirst _ x =
    x


{-| An empty `Props` object that will terminate the program instead of returning garbage if `Debug.todo` is passed as input.
-}
emptyAbort : { todo : String -> Never } -> Props
emptyAbort { todo } =
    Props
        { dict = Dict.empty
        , log =
            \x y ->
                let
                    _ =
                        todo ("In function " ++ x ++ ":\n" ++ y)
                in
                ()
        , broken = False
        }


{-| Without specifying a value for the `key`, force the `Props` to only
accept values of type `Bool` with `key`.

If the `Props` object already associates values of type `Bool` with the `key`,
this will have no effect.

-}
expectFlag : Key -> Props -> Props
expectFlag key (Props props) =
    if props.broken then
        Props props

    else
        case Dict.get key props.dict of
            Nothing ->
                Props { props | dict = props.dict |> Dict.insert key (Bool NotInitialized) }

            Just (Bool _) ->
                Props props

            Just stored ->
                Props { props | broken = True }
                    |> butFirst (reportInvalidExpecation "expectFlag" key stored (Bool NotInitialized) (Props props))


{-| Get the last value associated with `key`. This flag must have previously been **set**, not merely
initialized.

     Props.empty
        |> Props.setFlag "pink" True
        |> Props.getFlag "pink"       -- == True

     Props.empty
        |> Props.expectFlag "pink"
        |> Props.setFlag "pink" False
        |> Props.getFlag "pink"       -- == False

     Props.empty
        |> Props.setFlag "pink" True
        |> Props.setFlag "pink" False
        |> Props.getFlag "pink"       -- == False

     Props.empty
        |> Props.getFlag "pink"       -- XXX ERROR UNDEFINED

     Props.empty
        |> Props.expectFlag "pink"
        |> Props.getFlag "pink"       -- XXX ERROR UNDEFINED

-}
getFlag : Key -> Props -> Bool
getFlag key (Props props) =
    if props.broken then
        -- answer is undefined
        True

    else
        case Dict.get key props.dict of
            Just (Bool (Initialized value)) ->
                value

            stored ->
                -- answer is undefined
                True
                    |> butFirst (reportInvalidGet "getFlag" key stored (Bool NotInitialized) (Props props))


{-| Sets the value associated with `key`. This must be identential to the the type of all values
previously associated with `key`.

     Props.empty
        |> Props.setFlag "pink" False
        |> Props.setFlag "pink" True  -- == { pink: True }

     Props.empty
        |> Props.setInt "pink" 17
        |> Props.setFlag "pink" True  -- XXX ERROR UNDEFINED

-}
setFlag : Key -> Bool -> Props -> Props
setFlag key value (Props props) =
    if props.broken then
        Props props

    else
        case Dict.get key props.dict of
            Nothing ->
                Props { props | dict = props.dict |> Dict.insert key (Bool (Initialized value)) }

            Just (Bool _) ->
                Props { props | dict = props.dict |> Dict.insert key (Bool (Initialized value)) }

            Just stored ->
                Props { props | broken = True }
                    |> butFirst (reportInvalidSet "setFlag" key stored (Bool (Initialized value)) (Props props))


{-| Without specifying a value for the `key`, force the `Props` to only
accept values of type `Maybe Bool` with `key`.

If the `Props` object already associates values of type `Maybe Bool` with the `key`,
this will have no effect.

-}
expectMaybeBool : Key -> Props -> Props
expectMaybeBool key (Props props) =
    if props.broken then
        Props props

    else
        case Dict.get key props.dict of
            Nothing ->
                Props { props | dict = props.dict |> Dict.insert key (MaybeBool NotInitialized) }

            Just (MaybeBool _) ->
                Props props

            Just stored ->
                Props { props | broken = True }
                    |> butFirst (reportInvalidExpecation "expectMaybeBool" key stored (MaybeBool NotInitialized) (Props props))


{-| Get the last value associated with `key`. This flag must have previously been **set**, not merely
initialized.

     Props.empty
        |> Props.setMaybeBool "a" (Just True)
        |> Props.getMaybeBool "a"     -- == Just True

     Props.empty
        |> Props.expectMaybeBool "a"
        |> Props.setMaybeBool "a" (Just False)
        |> Props.getMaybeBool "a"     -- == Just False

     Props.empty
        |> Props.setMaybeBool "a" (Just False)
        |> Props.setMaybeBool "a" Nothing
        |> Props.getMaybeBool "a"     -- == Nothing

     Props.empty
        |> Props.getMaybeBool "a"     -- XXX ERROR UNDEFINED

     Props.empty
        |> Props.expectMaybeBool "a"
        |> Props.getMaybeBool "a"     -- XXX ERROR UNDEFINED

-}
getMaybeBool : Key -> Props -> Maybe Bool
getMaybeBool key (Props props) =
    if props.broken then
        -- answer is undefined
        Just False

    else
        case Dict.get key props.dict of
            Just (MaybeBool (Initialized value)) ->
                value

            stored ->
                -- answer is undefined
                Just True
                    |> butFirst (reportInvalidGet "getMaybeBool" key stored (MaybeBool NotInitialized) (Props props))


{-| Sets the value associated with `key`. This must be identential to the the type of all values
previously associated with `key`.

     Props.empty
        |> Props.setMaybeBool "a" (Just True)
        |> Props.setMaybeBool "a" Nothing
                                      -- == { a: Nothing }

     Props.empty
        |> Props.setFlag "a" True
        |> Props.setMaybeBool "a" Nothing
                                      -- XXX ERROR UNDEFINED

-}
setMaybeBool : Key -> Maybe Bool -> Props -> Props
setMaybeBool key value (Props props) =
    if props.broken then
        Props props

    else
        case Dict.get key props.dict of
            Nothing ->
                Props { props | dict = props.dict |> Dict.insert key (MaybeBool (Initialized value)) }

            Just (MaybeBool _) ->
                Props { props | dict = props.dict |> Dict.insert key (MaybeBool (Initialized value)) }

            Just stored ->
                Props { props | broken = True }
                    |> butFirst (reportInvalidSet "setMaybeBool" key stored (MaybeBool (Initialized value)) (Props props))


{-| Without specifying a value for the `key`, force the `Props` to only
accept values of type `Int` with `key`.

If the `Props` object already associates values of type `Int` with the `key`,
this will have no effect.

-}
expectInt : Key -> Props -> Props
expectInt key (Props props) =
    if props.broken then
        Props props

    else
        case Dict.get key props.dict of
            Nothing ->
                Props { props | dict = props.dict |> Dict.insert key (Int NotInitialized) }

            Just (Int _) ->
                Props props

            Just stored ->
                Props { props | broken = True }
                    |> butFirst (reportInvalidExpecation "expectInt" key stored (Int NotInitialized) (Props props))


{-| Get the last value associated with `key`. This flag must have previously been **set**, not merely
initialized.

     Props.empty
        |> Props.setInt "count" 77
        |> Props.getInt "count"       -- == 77

     Props.empty
        |> Props.expectInt "count"
        |> Props.setInt "count" 12
        |> Props.getInt "count"       -- == 12

     Props.empty
        |> Props.setInt "count" 33
        |> Props.setInt "count" 4
        |> Props.getInt "count"       -- == 4

     Props.empty
        |> Props.getInt "count"       -- XXX ERROR UNDEFINED

     Props.empty
        |> Props.expectInt "count"
        |> Props.getInt "count"       -- XXX ERROR UNDEFINED

-}
getInt : Key -> Props -> Int
getInt key (Props props) =
    if props.broken then
        -- answer is undefined
        12345

    else
        case Dict.get key props.dict of
            Just (Int (Initialized value)) ->
                value

            stored ->
                -- answer is undefined
                99
                    |> butFirst (reportInvalidGet "getInt" key stored (Int NotInitialized) (Props props))


{-| Sets the value associated with `key`. This must be identential to the the type of all values
previously associated with `key`.

     Props.empty
        |> Props.setInt "num" 33
        |> Props.setInt "num" -123    -- == { num: -123 }

     Props.empty
        |> Props.setString "num" "four"
        |> Props.setInt "num" 2       -- XXX ERROR UNDEFINED

-}
setInt : Key -> Int -> Props -> Props
setInt key value (Props props) =
    if props.broken then
        Props props

    else
        case Dict.get key props.dict of
            Nothing ->
                Props { props | dict = props.dict |> Dict.insert key (Int (Initialized value)) }

            Just (Int _) ->
                Props { props | dict = props.dict |> Dict.insert key (Int (Initialized value)) }

            Just stored ->
                Props { props | broken = True }
                    |> butFirst (reportInvalidSet "setInt" key stored (Int (Initialized value)) (Props props))


{-| Without specifying a value for the `key`, force the `Props` to only
accept values of type `Maybe Int` with `key`.

If the `Props` object already associates values of type `Maybe Int` with the `key`,
this will have no effect.

-}
expectMaybeInt : Key -> Props -> Props
expectMaybeInt key (Props props) =
    if props.broken then
        Props props

    else
        case Dict.get key props.dict of
            Nothing ->
                Props { props | dict = props.dict |> Dict.insert key (MaybeInt NotInitialized) }

            Just (MaybeInt _) ->
                Props props

            Just stored ->
                Props { props | broken = True }
                    |> butFirst (reportInvalidExpecation "expectMaybeInt" key stored (MaybeInt NotInitialized) (Props props))


{-| Get the last value associated with `key`. This flag must have previously been **set**, not merely
initialized.

     Props.empty
        |> Props.setMaybeInt "zip" (Just 10036)
        |> Props.getMaybeInt "zip"    -- == Just 10036

     Props.empty
        |> Props.expectMaybeInt "zip"
        |> Props.setMaybeInt "zip" (Just 89412)
        |> Props.getMaybeInt "zip"    -- == Just 89412

     Props.empty
        |> Props.setMaybeInt "zip" (Just 99705)
        |> Props.setMaybeInt "zip" Nothing
        |> Props.getMaybeInt "zip"    -- == Nothing

     Props.empty
        |> Props.getMaybeInt "zip"    -- XXX ERROR UNDEFINED

     Props.empty
        |> Props.expectMaybeInt "zip"
        |> Props.getMaybeInt "zip"    -- XXX ERROR UNDEFINED

-}
getMaybeInt : Key -> Props -> Maybe Int
getMaybeInt key (Props props) =
    if props.broken then
        -- answer is undefined
        Just -491

    else
        case Dict.get key props.dict of
            Just (MaybeInt (Initialized value)) ->
                value

            stored ->
                -- answer is undefined
                Just 3333333
                    |> butFirst (reportInvalidGet "getMaybeInt" key stored (MaybeInt NotInitialized) (Props props))


{-| Sets the value associated with `key`. This must be identential to the the type of all values
previously associated with `key`.

     Props.empty
        |> Props.setMaybeInt "b" (Just 99)
        |> Props.setMaybeInt "b" Nothing
                                      -- == { a: Nothing }

     Props.empty
        |> Props.setFlag "b" True
        |> Props.setMaybeInt "a" Nothing
                                      -- XXX ERROR UNDEFINED

-}
setMaybeInt : Key -> Maybe Int -> Props -> Props
setMaybeInt key value (Props props) =
    if props.broken then
        Props props

    else
        case Dict.get key props.dict of
            Nothing ->
                Props { props | dict = props.dict |> Dict.insert key (MaybeInt (Initialized value)) }

            Just (MaybeInt _) ->
                Props { props | dict = props.dict |> Dict.insert key (MaybeInt (Initialized value)) }

            Just stored ->
                Props { props | broken = True }
                    |> butFirst (reportInvalidSet "setMaybeInt" key stored (MaybeInt (Initialized value)) (Props props))


{-| Without specifying a value for the `key`, force the `Props` to only
accept values of type `String` with `key`.

If the `Props` object already associates values of type `String` with the `key`,
this will have no effect.

-}
expectString : Key -> Props -> Props
expectString key (Props props) =
    if props.broken then
        Props props

    else
        case Dict.get key props.dict of
            Nothing ->
                Props { props | dict = props.dict |> Dict.insert key (String NotInitialized) }

            Just (String _) ->
                Props props

            Just stored ->
                Props { props | broken = True }
                    |> butFirst (reportInvalidExpecation "expectString" key stored (String NotInitialized) (Props props))


{-| Get the last value associated with `key`. This flag must have previously been **set**, not merely
initialized.

     Props.empty
        |> Props.setString "name" "Gerald"
        |> Props.getString "name"     -- == "Gerald"

     Props.empty
        |> Props.expectString "name"
        |> Props.setString "name" "Nivedita"
        |> Props.getString "name"     -- == "Nivedita"

     Props.empty
        |> Props.setString "name" "Harold"
        |> Props.setString "name" "Arbob"
        |> Props.getString "name"     -- == "Arbob"

     Props.empty
        |> Props.getString "name"     -- XXX ERROR UNDEFINED

     Props.empty
        |> Props.expectString "name"
        |> Props.getString "name"     -- XXX ERROR UNDEFINED

-}
getString : Key -> Props -> String
getString key (Props props) =
    if props.broken then
        -- answer is undefined
        "ERROR GETSTRING CALLED ON A BROKEN OBJECT"

    else
        case Dict.get key props.dict of
            Just (String (Initialized value)) ->
                value

            stored ->
                -- answer is undefined
                "ERROR RETURNED FROM GETSTRING"
                    |> butFirst (reportInvalidGet "getString" key stored (String NotInitialized) (Props props))


{-| Sets the value associated with `key`. This must be identential to the the type of all values
previously associated with `key`.

     Props.empty
        |> Props.setString "c" "pink"
        |> Props.setString "c" "red"  -- == { c: "red" }

     Props.empty
        |> Props.setInt "c" 17
        |> Props.setString "c" "blue" -- XXX ERROR UNDEFINED

-}
setString : Key -> String -> Props -> Props
setString key value (Props props) =
    if props.broken then
        Props props

    else
        case Dict.get key props.dict of
            Nothing ->
                Props { props | dict = props.dict |> Dict.insert key (String (Initialized value)) }

            Just (String _) ->
                Props { props | dict = props.dict |> Dict.insert key (String (Initialized value)) }

            Just stored ->
                Props { props | broken = True }
                    |> butFirst (reportInvalidSet "setString" key stored (String (Initialized value)) (Props props))


{-| Without specifying a value for the `key`, force the `Props` to only
accept values of type `List String` with `key`.

If the `Props` object already associates values of type `List String` with the `key`,
this will have no effect.

-}
expectStrings : Key -> Props -> Props
expectStrings key (Props props) =
    if props.broken then
        Props props

    else
        case Dict.get key props.dict of
            Nothing ->
                Props { props | dict = props.dict |> Dict.insert key (Strings NotInitialized) }

            Just (Strings _) ->
                Props props

            Just stored ->
                Props { props | broken = True }
                    |> butFirst (reportInvalidExpecation "expectStrings" key stored (Strings NotInitialized) (Props props))


{-| Get the last value associated with `key`. This flag must have previously been **set**, not merely
initialized.

     Props.empty
        |> Props.setStrings "to" [ "Vee", "Bo" ]
        |> Props.getStrings "to"      -- == [ "Vee", "Bo" ]

     Props.empty
        |> Props.expectStrings "to"
        |> Props.setStrings "to" []
        |> Props.getStrings "to"     -- == []

     Props.empty
        |> Props.setStrings "to" [ "Han" ]
        |> Props.addString "to" "Me"
        |> Props.getStrings "to"     -- == [ "Me", "Han" ]

     Props.empty
        |> Props.getStrings "to"     -- XXX ERROR UNDEFINED

     Props.empty
        |> Props.expectStrings "to"
        |> Props.getStrings "to"     -- XXX ERROR UNDEFINED

-}
getStrings : Key -> Props -> List String
getStrings key (Props props) =
    if props.broken then
        -- answer is undefined
        [ "ERROR GETSTRING CALLED ON A BROKEN OBJECT" ]

    else
        case Dict.get key props.dict of
            Just (Strings (Initialized value)) ->
                value

            stored ->
                let
                    () =
                        reportInvalidGet "getStrings" key stored (Strings NotInitialized) (Props props)
                in
                -- answer is undefined
                [ "ERROR", "RETURNED", "FROM", "GETSTRING", key ]


{-| Sets the value associated with `key`. This must be identential to the the type of all values
previously associated with `key`.

     Props.empty
        |> Props.setStrings "q" [ "A", "B", "C" ]
        |> Props.setStrings "q" []    -- == { q: [] }


     Props.empty
        |> Props.setStrings "q" []
        |> Props.addString "q" "D"
        |> Props.setStrings "q" [ "E" ]
                                      -- == { q: [ "E" ] }

     Props.empty
        |> Props.setString "q" "How?"
        |> Props.setStrings "q" [ "Why?", "When?" ]
                                      -- XXX ERROR UNDEFINED

-}
setStrings : Key -> List String -> Props -> Props
setStrings key value (Props props) =
    if props.broken then
        Props props

    else
        case Dict.get key props.dict of
            Nothing ->
                Props { props | dict = props.dict |> Dict.insert key (Strings (Initialized value)) }

            Just (Strings _) ->
                Props { props | dict = props.dict |> Dict.insert key (Strings (Initialized value)) }

            Just stored ->
                Props { props | broken = True }
                    |> butFirst (reportInvalidSet "setStrings" key stored (Strings (Initialized value)) (Props props))


{-| Requires `key` to be associated with an initialized list of strings, and appends the
new value to the beginning of that list.
-}
addString : Key -> String -> Props -> Props
addString key value props =
    setStrings key (value :: getStrings key props) props


{-| Without specifying a value for the `key`, force the `Props` to only
accept values of type `Maybe String` with `key`.

If the `Props` object already associates values of type `Maybe String` with the `key`,
this will have no effect.

-}
expectMaybeString : Key -> Props -> Props
expectMaybeString key (Props props) =
    if props.broken then
        Props props

    else
        case Dict.get key props.dict of
            Nothing ->
                Props { props | dict = props.dict |> Dict.insert key (MaybeString NotInitialized) }

            Just (MaybeString _) ->
                Props props

            Just stored ->
                Props { props | broken = True }
                    |> butFirst (reportInvalidExpecation "expectMaybeString" key stored (MaybeString NotInitialized) (Props props))


{-| Get the last value associated with `key`. This flag must have previously been **set**, not merely
initialized.

     Props.empty
        |> Props.setMaybeString "kw" (Just "lost")
        |> Props.getMaybeString "kw"     -- == Just "lost"

     Props.empty
        |> Props.expectMaybeString "kw"
        |> Props.setMaybeString "kw" (Just "forbidden")
        |> Props.getMaybeString "kw"     -- == Just "forbidden"

     Props.empty
        |> Props.setMaybeString "kw" (Just "voluntary")
        |> Props.setMaybeString "kw" Nothing
        |> Props.getMaybeString "kw"  -- == Nothing

     Props.empty
        |> Props.getMaybeString "kw"  -- XXX ERROR UNDEFINED

     Props.empty
        |> Props.expectMaybeString "kw"
        |> Props.getMaybeString "kw"  -- XXX ERROR UNDEFINED

-}
getMaybeString : Key -> Props -> Maybe String
getMaybeString key (Props props) =
    if props.broken then
        -- answer is undefined
        Just "ERROR GETMAYBESTRING CALLED ON A BROKEN OBJECT"

    else
        case Dict.get key props.dict of
            Just (MaybeString (Initialized value)) ->
                value

            stored ->
                -- answer is undefined
                Just "ERROR GETMAYBESTRING CALLED ON A BROKEN OBJECT"
                    |> butFirst (reportInvalidGet "getMaybeString" key stored (MaybeString NotInitialized) (Props props))


{-| Sets the value associated with `key`. This must be identential to the the type of all values
previously associated with `key`.

     Props.empty
        |> Props.setMaybeString "x" (Just "lost")
        |> Props.setMaybeString "x" Nothing
                                      -- == { a: Nothing }

     Props.empty
        |> Props.setFlag "x" True
        |> Props.setMaybeString "x" Nothing
                                      -- XXX ERROR UNDEFINED

-}
setMaybeString : Key -> Maybe String -> Props -> Props
setMaybeString key value (Props props) =
    if props.broken then
        Props props

    else
        case Dict.get key props.dict of
            Nothing ->
                Props { props | dict = props.dict |> Dict.insert key (MaybeString (Initialized value)) }

            Just (MaybeString _) ->
                Props { props | dict = props.dict |> Dict.insert key (MaybeString (Initialized value)) }

            Just stored ->
                Props { props | broken = True }
                    |> butFirst (reportInvalidSet "setMaybeString" key stored (MaybeString (Initialized value)) (Props props))
