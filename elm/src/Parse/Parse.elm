module Parse.Parse exposing (..)

import Camperdown.Config.Config as Config
import Camperdown.Loc as Loc exposing (Loc)
import Camperdown.Occurs exposing (Occurs(..))
import Camperdown.Parse
import Camperdown.Parse.Syntax as Camp
import Camperdown.Problem as Problem
import Char.Extra as Char
import Dict exposing (Dict)
import Html exposing (b)
import List.Extra as List
import Maybe.Extra as Maybe
import Result.Extra as Result
import Script as S
import ScriptTypes as Script
import Set exposing (Set)
import Svg.Attributes exposing (in_, result)


resultFold : (a -> b -> Result x b) -> b -> List a -> Result x b
resultFold f y xs =
    case xs of
        [] ->
            Ok y

        x :: rest ->
            case f x y of
                Ok z ->
                    resultFold f z rest

                Err e ->
                    Err e


config : Config.ParserConfig
config =
    { verbatimOpts = Set.fromList [ '`' ]
    , annotationOpts =
        [ { startSymbol = "*", endSymbol = Just "*", commandOccursAfterwards = Never }
        , { startSymbol = "~", endSymbol = Just "~", commandOccursAfterwards = Never }
        , { startSymbol = "/", endSymbol = Just "/", commandOccursAfterwards = Never }
        , { startSymbol = "\"", endSymbol = Just "\"", commandOccursAfterwards = Never }
        , { startSymbol = "...", endSymbol = Nothing, commandOccursAfterwards = Never }
        , { startSymbol = "---", endSymbol = Nothing, commandOccursAfterwards = Never }
        , { startSymbol = "--", endSymbol = Nothing, commandOccursAfterwards = Never }
        , { startSymbol = "[", endSymbol = Just "]", commandOccursAfterwards = Always }
        ]
    , annotationFirstChars = Set.fromList [ '*', '/', '"', '[', '.', '-', '~' ]
    , meaningful = Set.fromList [ '\\', '[', ']', '\n', '.', '-', '`', '*', '/', '"', '~' ]
    , escapable = Set.fromList [ '\\', '"', '!', '?', '#', '[', ']', '(', ')', '.', '-', '`', '*', '/', '"', '&' ]
    , verbatimMarkers = [ "%" ]
    }


parse : String -> Result String { me : Script.AddressbookEntry, script : List Script.ThreadScript, starting : List String }
parse str =
    let
        { prelude, sections } =
            Camperdown.Parse.parse config str

        contactinfo =
            List.map preludeItem prelude
                |> Result.combine
                |> Result.map Dict.fromList
                |> Result.andThen
                    (\contacts ->
                        case Dict.get "Me" contacts of
                            Nothing ->
                                Err "There needs to be a `!contact Me` to define the player character."

                            Just me ->
                                Ok { me = me, contacts = contacts }
                    )

        rawemails : Result String (List ConvertedSection)
        rawemails =
            contactinfo
                |> Result.andThen
                    (\{ contacts } ->
                        List.map (convertSection contacts) sections
                            |> Result.combine
                    )

        rawscript : Result String (List { first : ConvertedSection, rest : List ConvertedSection })
        rawscript =
            rawemails
                |> Result.andThen
                    (resultFold
                        (\email x ->
                            case x of
                                Nothing ->
                                    if email.level == 1 then
                                        Ok <| Just { accum = [], first = email, rest = [] }

                                    else
                                        Err "The first section needs to start with a single `#` symbol."

                                Just { accum, first, rest } ->
                                    if email.level == 1 then
                                        Ok <| Just { accum = { first = first, rest = List.reverse rest } :: accum, first = email, rest = [] }

                                    else
                                        Ok <| Just { accum = accum, first = first, rest = email :: rest }
                        )
                        Nothing
                    )
                |> Result.map (Maybe.map (\{ accum, first, rest } -> List.reverse ({ first = first, rest = List.reverse rest } :: accum)))
                |> Result.map (Maybe.withDefault [])

        script : Result String (List Script.ThreadScript)
        script =
            rawscript
                |> Result.map
                    (List.map
                        (\{ first, rest } ->
                            { id = Loc.value first.name
                            , start = ""
                            , subject = Loc.value first.name
                            , scenes =
                                List.foldr (\{ name, scene } -> Dict.insert (Loc.value name) scene)
                                    (Dict.singleton "" first.scene)
                                    rest
                            }
                        )
                    )

        starting : Result String (List String)
        starting =
            rawemails
                |> Result.map
                    (List.foldl
                        (\{ level, name, impliedThreads, impliedScenes } ( currentSection, accum ) ->
                            let
                                newCurrentSection =
                                    if level == 1 then
                                        Loc.value name

                                    else
                                        currentSection

                                implied1 =
                                    List.foldr
                                        (\implied -> Dict.insert ( newCurrentSection, implied.name ) ( implied.at, implied.inSection ))
                                        accum.implied
                                        impliedScenes

                                implied2 =
                                    List.foldr
                                        (\implied -> Dict.insert ( implied.name, "" ) ( implied.at, implied.inSection ))
                                        implied1
                                        impliedThreads
                            in
                            if level == 1 then
                                ( Loc.value name
                                , { found = Dict.insert ( Loc.value name, "" ) (Loc.location name) accum.found
                                  , implied = implied2
                                  }
                                )

                            else
                                ( currentSection
                                , { found = Dict.insert ( currentSection, Loc.value name ) (Loc.location name) accum.found
                                  , implied = implied2
                                  }
                                )
                        )
                        ( "", { found = Dict.empty, implied = Dict.empty } )
                    )
                |> Result.map Tuple.second
                |> Result.andThen
                    (\{ found, implied } ->
                        resultFold
                            (\( ( thread, scene ), ( impliedAt, impliedSection ) ) accum ->
                                case Dict.get ( thread, scene ) found of
                                    Nothing ->
                                        if scene == "" then
                                            Err ("Thread titled \"" ++ thread ++ "\" implied to exist on line " ++ String.fromInt impliedAt.start.line ++ " in section \"" ++ Loc.value impliedSection ++ "\", but no such thread exists.")

                                        else
                                            Err ("Scene \"" ++ scene ++ "\" in thread \"" ++ thread ++ "\" implied to exist on line " ++ String.fromInt impliedAt.start.line ++ " in section \"" ++ Loc.value impliedSection ++ "\", but no such scene exists in that thread.")

                                    Just _ ->
                                        Ok accum
                            )
                            { found = found, implied = implied }
                            (Dict.toList implied)
                    )
                |> Result.andThen
                    (\{ found, implied } ->
                        resultFold
                            (\( ( thread, scene ), location ) accum ->
                                case Dict.get ( thread, scene ) implied of
                                    Nothing ->
                                        if scene == "" then
                                            Ok { accum | starting = accum.starting ++ [ thread ] }

                                        else
                                            Err ("Scene \"" ++ scene ++ "\" in thread \"" ++ thread ++ "\" (line " ++ String.fromInt location.start.line ++ ") is dead code. No other scene exists that would trigger it.")

                                    Just _ ->
                                        Ok accum
                            )
                            { found = found, implied = implied, starting = [] }
                            (Dict.toList found)
                    )
                |> Result.map .starting
    in
    Result.map3
        (\{ me } script_ starting_ ->
            { me = me
            , script = script_
            , starting = starting_
            }
        )
        contactinfo
        script
        starting


type alias ImpliedResource =
    { at : Loc.Location, name : String, inSection : Loc String }


type alias ConvertedSection =
    { level : Int
    , scene : Script.ThreadScene
    , name : Loc String
    , impliedThreads : List ImpliedResource
    , impliedScenes : List ImpliedResource
    }


convertSection : Dict String Script.AddressbookEntry -> Camp.Section -> Result String ConvertedSection
convertSection contacts { level, label, contents } =
    (case label of
        Camp.Anonymous line ->
            Err ("Section on line " ++ String.fromInt line ++ " needs to be give a name.")

        Camp.Named name ->
            Ok name
    )
        |> Result.andThen
            (\sectionName ->
                (let
                    err =
                        \() -> Err <| ("First command in section on line " ++ (sectionName |> Loc.location |> .start |> .line |> String.fromInt) ++ " is not an `!email` command with no arguments like I expected.")
                 in
                 case contents of
                    (Camp.Command { command }) :: rest ->
                        case command of
                            ( Just ( loc, "email" ), ( [], parameters ) ) ->
                                convertEmailParameters contacts loc parameters
                                    |> Result.map (\x -> ( x, rest ))

                            _ ->
                                err ()

                    (Camp.Problem { problem }) :: _ ->
                        Err problem

                    _ ->
                        err ()
                )
                    |> Result.andThen
                        (\( { from, to }, rest ) ->
                            rest
                                |> resultFold convertEmailElement { from = from, to = to, email = [], actions = [], scenes = [], threads = [] }
                        )
                    |> Result.map
                        (\{ from, to, email, actions, scenes, threads } ->
                            { level = level
                            , scene =
                                { receivedEmail = { from = from, to = to, contents = List.reverse email }
                                , actions = List.reverse actions
                                }
                            , name = sectionName
                            , impliedScenes = List.map (\scene -> { name = Loc.value scene, at = Loc.location scene, inSection = sectionName }) scenes
                            , impliedThreads = List.map (\thread -> { name = Loc.value thread, at = Loc.location thread, inSection = sectionName }) threads
                            }
                        )
            )


convertEmailParameters : Dict String Script.AddressbookEntry -> Loc.Location -> List (Loc Camp.Parameter) -> Result String { from : Script.AddressbookEntry, to : List Script.AddressbookEntry }
convertEmailParameters addressBook l parameters =
    resultFold
        (\parameter accum ->
            case parameter of
                ( loc, ( ( _, "from" ), [ ( varloc, Camp.Variable ident ) ] ) ) ->
                    if Maybe.isNothing accum.from then
                        case Dict.get ident addressBook of
                            Just address ->
                                Ok { accum | from = Just address }

                            Nothing ->
                                Err ("Email from unknown identity " ++ ident ++ " on line " ++ String.fromInt varloc.start.line)

                    else
                        Err ("On line " ++ String.fromInt loc.start.line ++ " encountered parameter `|> from " ++ ident ++ "`, but I already know who this email is from.")

                ( _, ( ( loc, "from" ), _ ) ) ->
                    Err ("Parameter `|> from` takes a single argument, an identifier for an contact in the address book (line " ++ String.fromInt loc.start.line ++ ")")

                ( _, ( ( _, "to" ), [ ( varloc, Camp.Variable ident ) ] ) ) ->
                    case Dict.get ident addressBook of
                        Just address ->
                            Ok { accum | to = address :: accum.to }

                        Nothing ->
                            Err ("Email from unknown identity " ++ ident ++ " on line " ++ String.fromInt varloc.start.line)

                ( _, ( ( loc, "to" ), _ ) ) ->
                    Err ("Parameter `|> to` takes a single argument, an identifier for an contact in the address book (line " ++ String.fromInt loc.start.line ++ ")")

                ( _, ( ( loc, param ), _ ) ) ->
                    Err ("Only expected `|> from` or `|> to` parameters, not `|> param` (line " ++ String.fromInt loc.start.line ++ ")")
        )
        { from = Nothing, to = [] }
        parameters
        |> Result.andThen
            (\accum ->
                case accum.from of
                    Nothing ->
                        Err ("No `|> from` command was found in the `! email` command on line " ++ String.fromInt l.start.line)

                    Just from ->
                        Ok { from = from, to = List.reverse accum.to }
            )


convertEmailElement : Camp.Element -> { a | email : List String } -> Result String { a | email : List String }
convertEmailElement element accum =
    case element of
        Camp.Paragraph { contents } ->
            List.map convertMarkup contents
                |> Result.combine
                |> Result.map String.concat
                |> Result.map (\paragraph -> { accum | email = paragraph :: accum.email })

        Camp.Command { command, lines } ->
            Ok accum

        -- Ignore!
        Camp.Problem { problem } ->
            Err problem

        Camp.Item { lines } ->
            Err ("Unexpected list item on line " ++ String.fromInt lines.start)

        Camp.Preformatted { lines } ->
            Err ("Unexpected preformatted section on line " ++ String.fromInt lines.start)


convertMarkup : Camp.Text -> Result String String
convertMarkup element =
    case element of
        Camp.Raw str ->
            Ok str

        Camp.Verbatim _ ( _, str ) ->
            Ok str

        Camp.Annotation _ markup _ _ ->
            List.map convertMarkup markup |> Result.combine |> Result.map String.concat

        Camp.InlineProblem problem ->
            Err (Problem.inlineToString problem |> Loc.value)


preludeItem : Camp.Element -> Result String ( String, Script.AddressbookEntry )
preludeItem item =
    case item of
        Camp.Preformatted { lines } ->
            Err <| "Did not expect preformatted section in the prelude."

        Camp.Item { lines } ->
            Err <| "Unexpected `:` item on line " ++ String.fromInt lines.start

        Camp.Paragraph _ ->
            Err <| "Did not expect paragraph text in the prelude."

        Camp.Problem { problem, lines } ->
            Err <| problem ++ " line " ++ String.fromInt lines.start

        Camp.Command { command, lines } ->
            case command of
                ( Just ( _, "contact" ), ( arguments, parameters ) ) ->
                    contactItem arguments parameters

                _ ->
                    Err <| "Only expect `!contact` commands in prelude, line " ++ String.fromInt lines.start


contactItem : List (Loc Camp.Value) -> List (Loc Camp.Parameter) -> Result String ( String, Script.AddressbookEntry )
contactItem arguments parameters =
    case arguments of
        [ ( _, Camp.Variable id ), ( _, Camp.String email ) ] ->
            List.foldr
                (\parameter ->
                    Result.andThen
                        (\data ->
                            case parameter of
                                ( _, ( ( _, "short" ), [ ( _, Camp.String short ) ] ) ) ->
                                    Ok { data | short = Just short }

                                ( _, ( ( _, "full" ), [ ( _, Camp.String full ) ] ) ) ->
                                    Ok { data | full = Just full }

                                _ ->
                                    Err "Bad parameter"
                        )
                )
                (Ok { full = Nothing, short = Nothing })
                parameters
                |> Result.map
                    (\{ full, short } ->
                        ( id
                        , case ( full, short ) of
                            ( Nothing, Nothing ) ->
                                let
                                    shorty =
                                        String.toList email
                                            |> List.takeWhile (\ch -> ch /= '@')
                                            |> String.fromList
                                in
                                { email = email, full = email, short = shorty }

                            ( Nothing, Just shorty ) ->
                                { email = email, full = shorty, short = shorty }

                            ( Just longy, Nothing ) ->
                                let
                                    shorty =
                                        String.toList longy
                                            |> List.takeWhile (Char.isSpace >> not)
                                            |> String.fromList
                                in
                                { email = email, full = longy, short = shorty }

                            ( Just longy, Just shorty ) ->
                                { email = email, full = longy, short = shorty }
                        )
                    )

        [] ->
            Err "Email address required in `!contact` command"

        _ ->
            Err "Too many arguments in `!contact` command"
