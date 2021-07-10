module App exposing (main)

import AppTypes as App
import Browser
import Cmd.Extra as Cmd
import Dict
import List.Extra as List
import Script as S
import ScriptTypes as Script
import View


main : Program () App.Model App.Msg
main =
    Browser.element
        { init = init
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


init : () -> ( App.Model, Cmd App.Msg )
init () =
    List.foldr
        (\scriptId model ->
            let
                script =
                    getScript scriptId model

                ( contents, responseOptions, archivable ) =
                    advanceThread model script script.start
            in
            { model
                | inbox =
                    { scriptId = scriptId
                    , contents = contents
                    , state = App.Unread { archivable = archivable, responseOptions = responseOptions }
                    }
                        :: model.inbox
            }
        )
        { state = App.InboxOpen
        , blocked = Nothing
        , inbox = []
        , scripts = S.myScript
        }
        S.starting
        |> Cmd.pure


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
            }



-- UPDATE


update : App.Msg -> App.Model -> ( App.Model, Cmd App.Msg )
update msg model =
    case msg of
        App.ReturnToInbox ->
            { model | state = App.InboxOpen }
                |> Cmd.pure

        App.OpenThread location ->
            let
                inboxWithRead =
                    -- Mark current thread as App.Unread
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
                        |> Cmd.pure

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

                                {- }
                                   case currentlySelectedOptionIndex of
                                       Nothing ->
                                           -- Should be impossible!
                                           ( thread, Nothing )

                                       Just index ->
                                           case List.getAt index responseOptions of
                                               Nothing ->
                                                   -- Should be impossible!
                                                   ( thread, Nothing )

                                               Just r -> r
                                                   ( { thread
                                                       | contents = thread.contents ++ [ response.email ]
                                                       , state = App.Responded { archivable = False }
                                                     }
                                                   , response.next
                                                   , r
                                                   )
                                -}
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

                        newThreads =
                            List.map
                                (\spawnId ->
                                    let
                                        script =
                                            getScript spawnId model

                                        ( contents, responseOptions, archivable ) =
                                            advanceThread model script script.start
                                    in
                                    { scriptId = spawnId
                                    , contents = contents
                                    , state =
                                        App.Unread
                                            { archivable = archivable
                                            , responseOptions = responseOptions
                                            }
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
                                            }
                                        |> advanceInbox model
                                   )
                        , blocked = response.next |> Maybe.map (\blockedNextId -> { scriptId = location.scriptId, next = blockedNextId })
                        , state = App.InboxOpen
                    }
                        |> Cmd.pure

                _ ->
                    -- Should be impossible!
                    model |> Cmd.pure


advanceInbox : App.Model -> List App.ActiveThread -> List App.ActiveThread
advanceInbox model inbox =
    case model.blocked of
        Nothing ->
            inbox

        Just { scriptId, next } ->
            case List.splitWhen (\thread -> thread.scriptId == scriptId) inbox of
                Just ( prefix, thread :: postfix ) ->
                    case advanceThread model (getScript scriptId model) next of
                        ( newEmails, responseOptions, archivable ) ->
                            { thread
                                | contents = thread.contents ++ newEmails
                                , state = App.Unread { archivable = archivable, responseOptions = responseOptions }
                            }
                                :: prefix
                                ++ postfix

                _ ->
                    -- Should be impossible! (scriptId not found in inbox)
                    inbox


advanceThread : App.Model -> Script.ThreadScript -> String -> ( List Script.Email, List Script.EmailResponse, Bool )
advanceThread model script next =
    Dict.get (Debug.log "next" next) script.scenes
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
                    responseOptions =
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
                            ( newEmails, action, archivable ) =
                                advanceThread model script goto
                        in
                        ( receivedEmail :: newEmails, action, archivable )

                    Nothing ->
                        -- Base case: this is the last email in the thread
                        -- extension
                        ( [ receivedEmail ], responseOptions, Debug.log "archivable" <| List.any ((==) Script.Archive) (Debug.log "Actions" actions) )
            )
        |> Maybe.withDefault ( [], [], False )



-- View
-- Subscriptions


subscriptions : App.Model -> Sub App.Msg
subscriptions _ =
    Sub.none
