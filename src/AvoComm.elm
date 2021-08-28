module AvoComm exposing (..)

import App as App exposing (Model)
import Browser
import Browser.Navigation as Nav exposing (Key)
import Cmd.Extra as Cmd
import CryptoScript
import Dict
import Dict.Extra as Dict
import Html exposing (text)
import List.Extra as List
import Markup exposing (Markup)
import Message exposing (Message)
import Props
import Props2 as Props
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
            (\threadId model ->
                let
                    threadScript =
                        getThreadScript threadId model

                    { newEmails, responseOptions, archivable, size } =
                        advanceThread model threadScript threadScript.start
                in
                { model
                    | inbox = { threadId = threadId } :: model.inbox
                    , threads =
                        model.threads
                            |> Dict.insert threadId
                                { threadId = threadId
                                , contents = newEmails
                                , state = App.Ready { responseOptions = responseOptions }
                                , props =
                                    App.emptyProps
                                        |> Props.setFlag "archivable" archivable
                                        |> Props.setFlag "archived" False
                                        |> Props.setInt "size" size
                                        |> Props.setFlag "starred" False
                                        |> Props.setMaybeBool "important" Nothing
                                        |> Props.setFlag "unread" True
                                        |> Props.setMaybeInt "selection" Nothing
                                        |> Props.setFlag "open" False
                                }
                }
            )
            { blocked = []
            , inbox = []
            , threads = Dict.empty
            , script = CryptoScript.hashScript script
            , navKey = key
            , attachment = Nothing
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


getThread : String -> App.Model -> App.ActiveThread
getThread threadId model =
    Dict.get threadId model.threads
        |> Maybe.withDefault
            { threadId = threadId
            , contents = []
            , state = App.Waiting
            , props = App.emptyProps
            }



-- UPDATE


setFlag : { threadId : String, key : String, value : Bool } -> App.Model -> App.Model
setFlag { threadId, key, value } model =
    { model
        | threads =
            model.threads |> Dict.update threadId (Maybe.map (\thread -> { thread | props = thread.props |> Props.setFlag key value }))
    }


setMaybeBool : { threadId : String, key : String, value : Maybe Bool } -> App.Model -> App.Model
setMaybeBool { threadId, key, value } model =
    { model
        | threads =
            model.threads |> Dict.update threadId (Maybe.map (\thread -> { thread | props = thread.props |> Props.setMaybeBool key value }))
    }


setMaybeInt : { threadId : String, key : String, value : Maybe Int } -> App.Model -> App.Model
setMaybeInt { threadId, key, value } model =
    { model
        | threads =
            model.threads |> Dict.update threadId (Maybe.map (\thread -> { thread | props = thread.props |> Props.setMaybeInt key value }))
    }


closeThread : App.Model -> App.Model
closeThread model =
    { model
        | threads =
            Dict.map
                (\_ thread ->
                    { thread
                        | props =
                            Props.setFlag "open" False thread.props
                    }
                )
                model.threads
    }


openThread : String -> App.Model -> App.Model
openThread id model =
    { model
        | threads =
            Dict.map
                (\threadId thread ->
                    { thread
                        | props =
                            if id == threadId then
                                thread.props
                                    |> Props.setFlag "open" True
                                    |> Props.setFlag "unread" False

                            else
                                thread.props
                                    |> Props.setFlag "open" False
                    }
                )
                model.threads
    }


update : App.Msg -> App.Model -> ( App.Model, Cmd App.Msg )
update msg model =
    case msg of
        App.DoNothing ->
            model |> Cmd.pure

        App.NavPushUrl url ->
            model |> Cmd.with (Nav.pushUrl model.navKey ("#" ++ url))

        App.NavBack ->
            model |> Cmd.with (Nav.back model.navKey 1)

        App.OnUrlChange url ->
            case Maybe.map (String.split "/") url.fragment of
                Just [ "", "k", "inbox", "" ] ->
                    model
                        |> closeThread
                        |> Cmd.pure

                Just [ "", "k", "inbox", id ] ->
                    model
                        |> openThread id
                        |> Cmd.pure

                _ ->
                    model |> Cmd.with (Nav.pushUrl model.navKey "#/k/inbox/")

        App.OnUrlRequest request ->
            case request of
                Browser.External url ->
                    let
                        _ =
                            Debug.log "ignoring exernal link" url
                    in
                    model |> Cmd.pure

                Browser.Internal url ->
                    case Maybe.map (String.split "/") url.fragment of
                        Just [ "", "k", "inbox", "" ] ->
                            model
                                |> closeThread
                                |> Cmd.pure

                        Just [ "", "k", "inbox", id ] ->
                            model
                                |> openThread id
                                |> Cmd.pure

                        _ ->
                            model 
                                |> Cmd.with (Nav.pushUrl model.navKey "#/k/inbox/")

        App.V viewMsg ->
            case viewMsg of
                App.Refresh ->
                    model
                        |> Cmd.pure

                App.OpenInbox ->
                    model
                        |> closeThread
                        |> Cmd.with (Nav.pushUrl model.navKey "#/k/inbox/")

                App.OpenThread { threadId } ->
                    model
                        |> setFlag { threadId = threadId, key = "open", value = True }
                        |> setFlag { threadId = threadId, key = "unread", value = False }
                        |> Cmd.with (Nav.pushUrl model.navKey ("#/k/inbox/" ++ threadId))

                App.Star { threadId, value } ->
                    model
                        |> setFlag { threadId = threadId, key = "starred", value = value }
                        |> Cmd.pure

                App.Important { threadId, value } ->
                    model
                        |> setMaybeBool { threadId = threadId, key = "important", value = Just value }
                        |> Cmd.pure

                App.Archive { threadId } ->
                    model
                        |> setFlag { threadId = threadId, key = "archived", value = True }
                        |> setFlag { threadId = threadId, key = "open", value = False }
                        |> Cmd.with (Nav.pushUrl model.navKey "#/k/inbox/")

                App.Recommendation { threadId, value } ->
                    model
                        |> setMaybeInt { threadId = threadId, key = "selection", value = value }
                        |> Cmd.pure

                App.Attachment attachment ->
                    { model | attachment = attachment }
                        |> Cmd.pure

                App.Select threadId responseIndex ->
                    let
                        thread =
                            getThread threadId model

                        response : Script.EmailResponse
                        response =
                            (case thread.state of
                                App.Waiting ->
                                    -- Should be impossible!
                                    Nothing

                                App.Ready { responseOptions } ->
                                    List.getAt responseIndex responseOptions
                            )
                                |> Maybe.withDefault
                                    -- Should be impossible to hit the default
                                    { shortText = []
                                    , email = { props = Props.empty, contents = [] }
                                    , next = Nothing
                                    , spawn = []
                                    }

                        newThreads : List App.ActiveThread
                        newThreads =
                            List.map
                                (\spawnedThreadId ->
                                    let
                                        script =
                                            getThreadScript spawnedThreadId model

                                        { newEmails, responseOptions, archivable, size } =
                                            advanceThread model script script.start
                                    in
                                    { threadId = spawnedThreadId
                                    , contents = newEmails
                                    , state =
                                        App.Ready
                                            { responseOptions = responseOptions }
                                    , props =
                                        App.emptyProps
                                            |> Props.setFlag "archivable" False
                                            |> Props.setFlag "archived" False
                                            |> Props.setInt "size" 5000
                                            -- TODO ^^^^^
                                            |> Props.setFlag "starred" False
                                            |> Props.setMaybeBool "important" Nothing
                                            |> Props.setFlag "unread" True
                                            |> Props.setMaybeInt "selection" Nothing
                                            |> Props.setFlag "open" False
                                    }
                                )
                                response.spawn

                        newInbox =
                            newThreads |> List.map (\spawnedThread -> { threadId = spawnedThread.threadId })
                    in
                    { model
                        | inbox = newInbox ++ model.inbox
                        , threads =
                            Dict.insert threadId
                                { thread
                                    | contents = thread.contents ++ [ response.email ]
                                    , state = App.Waiting
                                    , props =
                                        thread.props
                                            |> Props.setFlag "open" False
                                            |> Props.setInt "size"
                                                (Props.getInt "size" thread.props + 5000 {- TODO Props.getInt "size" response.email.props -})
                                }
                                model.threads

                        {- }
                                   -- Replace the appropriate thread with the
                                   -- updated thread
                                   |>
                                   |> List.setAt location.inboxIndex
                                       { thread
                                           | contents = thread.contents ++ [ response.email ]
                                           , state = App.Responded { archivable = False }
                                           , size =
                                               Props.getMaybeInt "size" response.email.props
                                                   |> Maybe.withDefault 4000
                                       }
                                   |> advanceInbox model

                           )
                        -}
                        , blocked =
                            response.next
                                |> Maybe.map (\blockedNextId -> [ { threadId = threadId, next = blockedNextId, delay = 1 } ])
                                |> Maybe.withDefault []
                                |> (++) model.blocked
                    }
                        |> advanceInbox
                        |> Cmd.with (Nav.pushUrl model.navKey "#/k/inbox/")


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
    List.foldr (\to n -> 75 + String.length to + n)
        (2048 + messageElementsSize contents + String.length (Props.getString "from" props))
        (Props.getStrings "to" props)


thereAreReadyThreads : App.Model -> Bool
thereAreReadyThreads model =
    Dict.any
        (\_ { state } ->
            case state of
                App.Waiting ->
                    False

                App.Ready { responseOptions } ->
                    responseOptions /= []
        )
        model.threads


advanceInbox : App.Model -> App.Model
advanceInbox =
    (\model ->
        let
            results =
                List.foldr
                    (\item { blocked, unblocked } ->
                        if item.delay == 0 then
                            { blocked = blocked, unblocked = item :: unblocked }

                        else
                            { blocked = { item | delay = item.delay - 1 } :: blocked, unblocked = unblocked }
                    )
                    { blocked = [], unblocked = [] }
                    model.blocked
        in
        List.foldr
            (\{ threadId, next } -> addNextSceneToThread threadId next)
            { model | blocked = results.blocked }
            results.unblocked
    )
        >> (\model ->
                if not (thereAreReadyThreads model) && model.blocked /= [] then
                    advanceInbox model

                else
                    model
           )


addNextSceneToThread : String -> String -> App.Model -> App.Model
addNextSceneToThread threadId next model =
    case List.splitWhen (\thread -> thread.threadId == threadId) model.inbox of
        Just ( prefix, updatedThread :: postfix ) ->
            let
                { newEmails, responseOptions, archivable, size } =
                    advanceThread model (getThreadScript threadId model) next
            in
            { model
                | inbox = updatedThread :: prefix ++ postfix
                , threads =
                    Dict.update threadId
                        (Maybe.map
                            (\thread ->
                                { thread
                                    | contents = thread.contents ++ newEmails
                                    , state = App.Ready { responseOptions = responseOptions }
                                    , props =
                                        thread.props
                                            |> Props.setFlag "archivable" archivable
                                            |> Props.setFlag "unread" True
                                            |> Props.setMaybeInt "selection" Nothing
                                            |> Props.setInt "size" (Props.getInt "size" thread.props + size)
                                }
                            )
                        )
                        model.threads
            }

        _ ->
            -- TODO: make this the spawn case????
            model



{- }
   case model.blocked of
       Nothing ->
           inbox

       Just { threadId, next } ->
           case List.splitWhen (\thread -> thread.threadId == threadId) inbox of
               Just ( prefix, thread :: postfix ) ->
                   case advanceThread model (getThreadScript threadId model) next of
                           { thread
                               | contents = thread.contents ++ newEmails
                               , state = App.Unread { archivable = archivable, responseOptions = responseOptions }
                               , size = thread.size + size
                           }
                               :: prefix
                               ++ postfix

               _ ->
                   -- Should be impossible! (threadId not found in inbox)
                   inbox
-}


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
