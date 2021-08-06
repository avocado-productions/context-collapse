module AvoComm exposing (..)

import App as App
import Browser.Navigation as Nav exposing (Key)
import Cmd.Extra as Cmd
import CryptoScript
import Dict
import Html exposing (text)
import List.Extra as List
import Markup exposing (Markup)
import Message exposing (Message)
import Script exposing (Script)
import Url exposing (Url)


init :
    Script
    -> Url
    -> Key
    -> ( App.Model, Cmd App.Msg )
init script _ key =
    script.starting
        |> List.map CryptoScript.hash
        |> List.foldr
            (\scriptId model ->
                let
                    threadScript =
                        getThreadScript scriptId model

                    { newEmails, responseOptions, archivable, size } =
                        advanceThread model threadScript threadScript.start
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
            , script = CryptoScript.hashScript script
            , navKey = key
            }
        |> Cmd.with (Nav.pushUrl key "#/k/inbox/")


getThreadScript : String -> App.Model -> Script.ThreadScript
getThreadScript needle model =
    List.find (\{ id } -> needle == id) model.script.threads
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
            model |> Cmd.with (Nav.pushUrl model.navKey ("#" ++ url))

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
                        |> Cmd.with (Nav.pushUrl model.navKey "#/k/inbox/")

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
                                    { shortText = []
                                    , email = { props = [], contents = [] }
                                    , next = Nothing
                                    , spawn = []
                                    }

                        newThreads : List App.ActiveThread
                        newThreads =
                            List.map
                                (\spawnId ->
                                    let
                                        script =
                                            getThreadScript spawnId model

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
                                                , size =
                                                    List.findMap
                                                        (\{ key, value } ->
                                                            if key == "size" then
                                                                String.toInt value

                                                            else
                                                                Nothing
                                                        )
                                                        response.email.props
                                                        |> Maybe.withDefault 4000
                                            }
                                        |> advanceInbox model
                                   )
                        , blocked = response.next |> Maybe.map (\blockedNextId -> { scriptId = location.scriptId, next = blockedNextId })
                        , state = App.InboxOpen
                    }
                        |> Cmd.with (Nav.pushUrl model.navKey "#/k/inbox/")

                _ ->
                    -- Should be impossible!
                    model |> Cmd.pure

        App.OnUrlChange url ->
            case Maybe.map (String.split "/") url.fragment of
                Just [ "", "k", "inbox", "" ] ->
                    model |> Cmd.with (Cmd.perform App.OpenInbox)

                Just [ "", "k", "inbox", id ] ->
                    List.indexedMap
                        (\inboxIndex { scriptId } ->
                            if id == scriptId then
                                Just (Cmd.perform (App.OpenThread { scriptId = scriptId, inboxIndex = inboxIndex }))

                            else
                                Nothing
                        )
                        model.inbox
                        |> List.filterMap identity
                        |> Cmd.batch
                        |> Tuple.pair model

                _ ->
                    model |> Cmd.with (Cmd.perform App.OpenInbox)

        App.OnUrlRequest _ ->
            model |> Cmd.pure


messageTextSize : Markup.Text -> Int
messageTextSize text =
    case text of
        Markup.Raw _ contents ->
            String.length contents

        Markup.Link { url, contents } ->
            messageMarkupSize contents + String.length url + 500


messageMarkupSize : Markup -> Int
messageMarkupSize =
    List.foldr (\text n -> messageTextSize text + n) 0


messageElementSize : Message.Element -> Int
messageElementSize element =
    case element of
        Message.Image { url } ->
            150000 + String.length url * 3

        Message.Paragraph contents ->
            messageMarkupSize contents

        Message.Quote contents ->
            500 + messageElementsSize contents


messageElementsSize : List Message.Element -> Int
messageElementsSize =
    List.foldr (\element n -> messageElementSize element + n) 0


messageSize : Message -> Int
messageSize { props, contents } =
    List.foldr (\{ key, value } n -> String.length key + String.length value + n)
        (2048 + messageElementsSize contents)
        props


advanceInbox : App.Model -> List App.ActiveThread -> List App.ActiveThread
advanceInbox model inbox =
    case model.blocked of
        Nothing ->
            inbox

        Just { scriptId, next } ->
            case List.splitWhen (\thread -> thread.scriptId == scriptId) inbox of
                Just ( prefix, thread :: postfix ) ->
                    case advanceThread model (getThreadScript scriptId model) next of
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
    -> { newEmails : List Message, responseOptions : List Script.EmailResponse, archivable : Bool, size : Int }
advanceThread model script next =
    Dict.get next script.scenes
        |> Maybe.map
            (\{ receivedEmail, actions } ->
                let
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
                        , size = size + messageSize receivedEmail
                        }

                    Nothing ->
                        -- Base case: this is the last email in the thread
                        -- extension
                        { newEmails = [ receivedEmail ]
                        , responseOptions = currentResponseOptions
                        , archivable = List.any ((==) Script.Archive) actions
                        , size = messageSize receivedEmail
                        }
            )
        |> Maybe.withDefault { newEmails = [], responseOptions = [], archivable = False, size = 0 }
