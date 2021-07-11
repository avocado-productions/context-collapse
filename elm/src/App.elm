module App exposing (main)

import AppTypes as App
import Browser
import Browser.Navigation as Nav exposing (Key)
import Cmd.Extra as Cmd
import CryptoScript
import Dict
import List.Extra as List
import Script as S
import ScriptTypes as Script
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, s)
import View


main : Program () App.Model App.Msg
main =
    Browser.application
        { init = init
        , view = View.view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = App.OnUrlRequest
        , onUrlChange = App.OnUrlChange
        }



-- MODEL


init : () -> Url -> Key -> ( App.Model, Cmd App.Msg )
init () url key =
    S.starting
        |> List.map CryptoScript.hash
        |> List.foldr
            (\scriptId model ->
                let
                    script =
                        getScript scriptId model

                    { newEmails, responseOptions, archivable, size } =
                        advanceThread model script script.start
                in
                { model
                    | inbox =
                        { scriptId = scriptId
                        , contents = newEmails
                        , state = App.Unread { archivable = archivable, responseOptions = responseOptions }
                        , starred = False
                        , size = size
                        }
                            :: model.inbox
                }
            )
            { state = App.InboxOpen
            , blocked = Nothing
            , inbox = []
            , scripts = S.myScript |> List.map CryptoScript.hashScript
            , navKey = key
            , me = S.me
            }
        |> Cmd.with (Nav.pushUrl key "/k/inbox/")


getScript : String -> App.Model -> Script.ThreadScript
getScript needle model =
    List.find (\script -> needle == script.id) model.scripts
        |> Maybe.withDefault
            { id = needle
            , subject = "Error, can't find " ++ needle
            , scenes = Dict.empty
            , start = "lol"
            }


getThread : Int -> App.Model -> App.ActiveThread
getThread index model =
    List.getAt index model.inbox
        |> Maybe.withDefault
            { scriptId = String.fromInt index
            , contents = []
            , state = App.Responded { archivable = False }
            , starred = False
            , size = -1025
            }



-- UPDATE


update : App.Msg -> App.Model -> ( App.Model, Cmd App.Msg )
update msg model =
    case msg of
        App.DoNothing ->
            model |> Cmd.pure

        App.NavBack ->
            model |> Cmd.with (Nav.back model.navKey 1)

        App.NavPushUrl url ->
            model |> Cmd.with (Nav.pushUrl model.navKey url)

        App.OpenInbox ->
            { model | state = App.InboxOpen } |> Cmd.pure

        App.OpenThread location ->
            let
                inboxWithRead =
                    -- Mark current thread as Unread
                    List.map
                        (\thread ->
                            if thread.scriptId /= location.scriptId then
                                thread

                            else
                                case thread.state of
                                    App.Unread { archivable, responseOptions } ->
                                        { thread
                                            | state =
                                                case responseOptions of
                                                    [] ->
                                                        App.Responded { archivable = archivable }

                                                    _ ->
                                                        App.Unresponded
                                                            { archivable = archivable
                                                            , responseOptions = responseOptions
                                                            , currentlySelectedOptionIndex = Nothing
                                                            }
                                        }

                                    _ ->
                                        thread
                        )
                        model.inbox
            in
            { model
                | inbox = inboxWithRead
                , state = App.ThreadOpen { location = location }
            }
                |> Cmd.pure

        App.ToggleStar scriptId ->
            { model
                | inbox =
                    List.map
                        (\thread ->
                            if thread.scriptId == scriptId then
                                { thread | starred = not thread.starred }

                            else
                                thread
                        )
                        model.inbox
            }
                |> Cmd.pure

        App.ToggleSuggestion n ->
            case model.state of
                App.ThreadOpen { location } ->
                    { model
                        | inbox =
                            List.indexedMap
                                (\inboxIndex thread ->
                                    if inboxIndex /= location.inboxIndex then
                                        thread

                                    else
                                        case thread.state of
                                            App.Unresponded state ->
                                                if state.currentlySelectedOptionIndex == Just n then
                                                    { thread | state = App.Unresponded { state | currentlySelectedOptionIndex = Nothing } }

                                                else
                                                    { thread | state = App.Unresponded { state | currentlySelectedOptionIndex = Just n } }

                                            _ ->
                                                thread
                                )
                                model.inbox
                    }
                        |> Cmd.pure

                _ ->
                    -- Should be impossible!
                    model |> Cmd.pure

        App.ArchiveThread ->
            case model.state of
                App.ThreadOpen { location } ->
                    { model
                        | inbox =
                            model.inbox
                                |> List.updateAt location.inboxIndex
                                    (\thread -> { thread | state = App.Archived })
                                |> advanceInbox model
                        , state = App.InboxOpen
                        , blocked = Nothing -- inbox has been advanced
                    }
                        |> Cmd.with (Nav.pushUrl model.navKey "/k/inbox/")

                _ ->
                    -- Should be impossible!
                    model |> Cmd.pure

        App.SelectSuggestion ->
            case model.state of
                App.ThreadOpen { location } ->
                    let
                        thread =
                            getThread location.inboxIndex model

                        response : Script.EmailResponse
                        response =
                            -- Append the selected email to the end of its thread,
                            -- change that thread's state to App.Responded
                            (case thread.state of
                                App.Unresponded { responseOptions, currentlySelectedOptionIndex } ->
                                    currentlySelectedOptionIndex
                                        |> Maybe.andThen (\index -> List.getAt index responseOptions)

                                _ ->
                                    -- Should be impossible!
                                    Nothing
                            )
                                |> Maybe.withDefault
                                    -- Should be impossible to hit the default
                                    { shortText = ""
                                    , email = { from = S.me, to = [], contents = [] }
                                    , next = Nothing
                                    , spawn = []
                                    }

                        newThreads : List App.ActiveThread
                        newThreads =
                            List.map
                                (\spawnId ->
                                    let
                                        script =
                                            getScript spawnId model

                                        { newEmails, responseOptions, archivable, size } =
                                            advanceThread model script script.start
                                    in
                                    { scriptId = spawnId
                                    , contents = newEmails
                                    , state =
                                        App.Unread
                                            { archivable = archivable
                                            , responseOptions = responseOptions
                                            }
                                    , starred = False
                                    , size = size
                                    }
                                )
                                response.spawn
                    in
                    { model
                        | inbox =
                            newThreads
                                ++ (model.inbox
                                        -- Replace the appropriate thread with the
                                        -- updated thread
                                        |> List.setAt location.inboxIndex
                                            { thread
                                                | contents = thread.contents ++ [ response.email ]
                                                , state = App.Responded { archivable = False }
                                                , size = List.foldr (\str size -> size + 2 * String.length str) thread.size response.email.contents
                                            }
                                        |> advanceInbox model
                                   )
                        , blocked = response.next |> Maybe.map (\blockedNextId -> { scriptId = location.scriptId, next = blockedNextId })
                        , state = App.InboxOpen
                    }
                        |> Cmd.with (Nav.pushUrl model.navKey "/k/inbox/")

                _ ->
                    -- Should be impossible!
                    model |> Cmd.pure

        App.OnUrlChange url ->
            if url.path == currentUrl model then
                model |> Cmd.pure

            else
                case Url.Parser.parse (parseUrl model) url of
                    Nothing ->
                        model |> Cmd.with (Cmd.perform App.OpenInbox)

                    Just route ->
                        model |> Cmd.with (Cmd.perform route)

        App.OnUrlRequest url ->
            let
                _ =
                    Debug.log "urlRequest" url
            in
            model |> Cmd.pure


currentUrl : App.Model -> String
currentUrl model =
    case model.state of
        App.InboxOpen ->
            "/k/inbox/"

        App.ThreadOpen { location } ->
            "/k/inbox/" ++ location.scriptId


parseUrl : App.Model -> Parser (App.Msg -> a) a
parseUrl model =
    let
        parseThreadId id =
            List.indexedMap
                (\inboxIndex { scriptId } ->
                    if id == scriptId then
                        Just <| App.OpenThread { scriptId = scriptId, inboxIndex = inboxIndex }

                    else
                        Nothing
                )
                model.inbox
                |> List.filterMap identity
                |> List.head
    in
    Url.Parser.oneOf
        [ s "k" </> s "inbox" </> Url.Parser.map App.OpenInbox Url.Parser.top
        , s "k" </> s "inbox" </> Url.Parser.custom "THREAD" parseThreadId
        ]


advanceInbox : App.Model -> List App.ActiveThread -> List App.ActiveThread
advanceInbox model inbox =
    case model.blocked of
        Nothing ->
            inbox

        Just { scriptId, next } ->
            case List.splitWhen (\thread -> thread.scriptId == scriptId) inbox of
                Just ( prefix, thread :: postfix ) ->
                    case advanceThread model (getScript scriptId model) next of
                        { newEmails, responseOptions, archivable, size } ->
                            { thread
                                | contents = thread.contents ++ newEmails
                                , state = App.Unread { archivable = archivable, responseOptions = responseOptions }
                                , size = thread.size + size
                            }
                                :: prefix
                                ++ postfix

                _ ->
                    -- Should be impossible! (scriptId not found in inbox)
                    inbox


advanceThread :
    App.Model
    -> Script.ThreadScript
    -> String
    -> { newEmails : List Script.Email, responseOptions : List Script.EmailResponse, archivable : Bool, size : Int }
advanceThread model script next =
    Dict.get next script.scenes
        |> Maybe.map
            (\{ receivedEmail, actions } ->
                let
                    newSize =
                        List.foldr (\text size -> String.length text + size)
                            (1500 + 500 * List.length receivedEmail.to)
                            receivedEmail.contents

                    -- Find an Immediate action (if one exists) from the script
                    immediateAction =
                        List.findMap
                            (\action ->
                                case action of
                                    Script.Immediate goto ->
                                        Just goto

                                    _ ->
                                        Nothing
                            )
                            actions

                    -- Find all Respond actions from the script
                    currentResponseOptions =
                        List.filterMap
                            (\action ->
                                case action of
                                    Script.Respond response ->
                                        Just response

                                    _ ->
                                        Nothing
                            )
                            actions
                in
                case immediateAction of
                    Just goto ->
                        -- This recursive call allows many new emails
                        -- to be resolved in a thread by way of
                        -- the Immediate action.
                        let
                            { newEmails, responseOptions, archivable, size } =
                                advanceThread model script goto
                        in
                        { newEmails = receivedEmail :: newEmails
                        , responseOptions = responseOptions
                        , archivable = archivable
                        , size = size + newSize
                        }

                    Nothing ->
                        -- Base case: this is the last email in the thread
                        -- extension
                        { newEmails = [ receivedEmail ]
                        , responseOptions = currentResponseOptions
                        , archivable = List.any ((==) Script.Archive) actions
                        , size = newSize
                        }
            )
        |> Maybe.withDefault { newEmails = [], responseOptions = [], archivable = False, size = 0 }
