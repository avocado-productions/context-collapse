module Parse.Parse exposing (..)

import Camperdown.Config.Config as Config
import Camperdown.Loc as Loc exposing (Loc)
import Camperdown.Occurs exposing (Occurs(..))
import Camperdown.Parse
import Camperdown.Parse.Syntax as Camp
import Camperdown.Problem as Problem
import Char.Extra as Char
import Dict exposing (Dict)
import List.Extra as List
import Maybe.Extra as Maybe
import Result.Extra as Result
import ScriptTypes as Script
import Set


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
                    (\{ me, contacts } ->
                        List.map (convertSection me contacts) sections
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
        |> Debug.log "wee"


type alias ImpliedResource =
    { at : Loc.Location, name : String, inSection : Loc String }


type alias ConvertedSection =
    { level : Int
    , scene : Script.ThreadScene
    , name : Loc String
    , impliedThreads : List ImpliedResource
    , impliedScenes : List ImpliedResource
    }


convertSection : Script.AddressbookEntry -> Dict String Script.AddressbookEntry -> Camp.Section -> Result String ConvertedSection
convertSection me contacts { level, label, contents } =
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
                                |> resultFold (convertEmailElement me contacts) { from = from, to = to, email = [], actions = [], scenes = [], threads = [] }
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


convertActionParameters : Dict String Script.AddressbookEntry -> Bool -> List (Loc Camp.Parameter) -> Result String { spawn : List (Loc String), next : Maybe (Loc String), to : List Script.AddressbookEntry }
convertActionParameters addressBook hasTo parameters =
    resultFold
        (\parameter accum ->
            case parameter of
                ( _, ( ( loc, "to" ), [ ( varloc, Camp.Variable ident ) ] ) ) ->
                    case Dict.get ident addressBook of
                        Just address ->
                            if hasTo then
                                Ok { accum | to = address :: accum.to }

                            else
                                Err ("Did not expect `|> to` parameters in this command (line " ++ String.fromInt loc.start.line ++ ")")

                        Nothing ->
                            Err ("Email from unknown identity " ++ ident ++ " on line " ++ String.fromInt varloc.start.line)

                ( _, ( ( loc, "to" ), _ ) ) ->
                    Err ("Parameter `|> to` takes a single argument, an identifier for an contact in the address book (line " ++ String.fromInt loc.start.line ++ ")")

                ( _, ( ( _, "spawns" ), [ ( loc, Camp.String str ) ] ) ) ->
                    Ok { accum | spawn = ( loc, str ) :: accum.spawn }

                ( _, ( ( loc, "spawns" ), _ ) ) ->
                    Err ("Parameter `|> spawn` takes a single argument, a string referencing the thread to be spawned (line " ++ String.fromInt loc.start.line ++ ")")

                ( _, ( ( _, "triggers" ), [ ( loc, Camp.String str ) ] ) ) ->
                    case accum.trigger of
                        Nothing ->
                            Ok { accum | trigger = Just ( loc, str ) }

                        Just ( loc2, str2 ) ->
                            Err ("An action can trigger at most one scene within the thread, but this action tries to trigger scene \"" ++ str2 ++ "\" on line " ++ String.fromInt loc2.start.line ++ " and then \"" ++ str ++ "\" on line " ++ String.fromInt loc.start.line)

                ( _, ( ( loc, "triggers" ), _ ) ) ->
                    Err ("Parameter `|> trigger` takes a single argument, a string referencing the scene to be spawned (line " ++ String.fromInt loc.start.line ++ ")")

                ( _, ( ( loc, param ), _ ) ) ->
                    let
                        expected =
                            if hasTo then
                                "`|> spawns`, `|> triggers`, or `|> to`"

                            else
                                "`|> spawns` or `|> triggers`"
                    in
                    Err ("Only expected " ++ expected ++ " parameters, not `|> " ++ param ++ "` (line " ++ String.fromInt loc.start.line ++ ")")
        )
        { spawn = [], trigger = Nothing, to = [] }
        parameters
        |> Result.map
            (\{ spawn, trigger, to } ->
                { next = trigger
                , spawn = List.reverse spawn
                , to = List.reverse to
                }
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
                    Err ("Only expected `|> from` or `|> to` parameters, not `|> " ++ param ++ "` (line " ++ String.fromInt loc.start.line ++ ")")
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


type alias EmailConvert a =
    { a
        | email : List String
        , threads : List (Loc String)
        , scenes : List (Loc String)
        , actions : List Script.Action
    }


convertEmailElement : Script.AddressbookEntry -> Dict String Script.AddressbookEntry -> Camp.Element -> EmailConvert a -> Result String (EmailConvert a)
convertEmailElement me contacts element accum =
    case element of
        Camp.Paragraph { contents } ->
            List.map convertMarkup contents
                |> Result.combine
                |> Result.map String.concat
                |> Result.map (\paragraph -> { accum | email = paragraph :: accum.email })

        Camp.Command { command, lines, child } ->
            case command of
                ( Just ( loc, "archive" ), ( [], [] ) ) ->
                    {- }
                       case child of
                           Nothing ->
                               convertActionParameters params
                                   |> Result.map (\{threads, scenes, actions} -> Ok { accum | threads = threads ++ accum.threads
                                   ,  scenes = scenes ++ accum.scenes,})

                           Just _ ->
                               Err ("!archive command should not have a chevron (>>, vv, or ->), line " ++ String.fromInt loc.start.line)
                    -}
                    Ok { accum | actions = Script.Archive :: accum.actions }

                ( Just ( loc, "archive" ), ( _, _ ) ) ->
                    Err ("!archive shouldn't have arguments or parameters (line " ++ String.fromInt loc.start.line ++ ")")

                ( Just ( _, "trigger" ), ( [ ( loc, Camp.String scene ) ], [] ) ) ->
                    Ok
                        { accum
                            | actions = Script.Immediate scene :: accum.actions
                            , scenes = ( loc, scene ) :: accum.scenes
                        }

                ( Just ( _, "respond" ), ( [ ( _, Camp.Markup text ) ], params ) ) ->
                    let
                        shortResponse_r =
                            List.map convertMarkup text
                                |> Result.combine
                                |> Result.map String.concat

                        emailResponse_r =
                            case child of
                                Nothing ->
                                    Err "!respond must have a cheveron (>> or vv) to include the actual email"

                                Just (Camp.Reference ( rloc, str )) ->
                                    Err ("Named diverts like \"-> " ++ str ++ "\" not allowed (line " ++ String.fromInt rloc.start.line ++ ")")

                                Just (Camp.Immediate response) ->
                                    convertEmailResponse response

                                Just (Camp.Nested response) ->
                                    convertEmailResponse response
                    in
                    Result.map3
                        (\shortResponse emailResponse { to, next, spawn } ->
                            { accum
                                | actions =
                                    Script.Respond
                                        { shortText = shortResponse
                                        , email = { to = to, from = me, contents = emailResponse }
                                        , next = Maybe.map Loc.value next
                                        , spawn = List.map Loc.value spawn
                                        }
                                        :: accum.actions
                                , threads = spawn ++ accum.threads
                                , scenes = Maybe.toList next ++ accum.scenes
                            }
                        )
                        shortResponse_r
                        emailResponse_r
                        (convertActionParameters contacts True params)

                ( Just ( loc, "respond" ), ( _, params ) ) ->
                    Err ("!respond should only have a single argument, markup text in brackets [like this] (line " ++ String.fromInt loc.start.line ++ ")")

                ( _, _ ) ->
                    Err ("Unexpected command on line " ++ String.fromInt lines.start ++ ", only expected to see !respond, !archive, or !trigger.")

        -- Ignore!
        Camp.Problem { problem } ->
            Err problem

        Camp.Item { lines } ->
            Err ("Unexpected list item on line " ++ String.fromInt lines.start)

        Camp.Preformatted { lines } ->
            Err ("Unexpected preformatted section on line " ++ String.fromInt lines.start)


convertEmailResponse : List Camp.Element -> Result String (List String)
convertEmailResponse elems =
    List.map
        (\elem ->
            case elem of
                Camp.Paragraph { contents } ->
                    List.map convertMarkup contents |> Result.combine |> Result.map String.concat

                Camp.Command { lines } ->
                    Err ("Unexpected command on line " ++ String.fromInt lines.start)

                Camp.Problem { problem } ->
                    Err problem

                Camp.Item { lines } ->
                    Err ("Unexpected list item on line " ++ String.fromInt lines.start)

                Camp.Preformatted { lines } ->
                    Err ("Unexpected preformatted section on line " ++ String.fromInt lines.start)
        )
        elems
        |> Result.combine


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
