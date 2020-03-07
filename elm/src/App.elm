module App exposing (main)

import AppTypes as App
import Browser
import Cmd.Extra as C
import Delay
import Dict
import Example
import List.Extra
import ScriptTypes as Script
import Set
import Util
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

initScripts : List Script.ThreadScript -> List App.ThreadScript
initScripts =
    List.indexedMap
        (\index script ->
            { subject = script.subject
            , scenes = script.scenes
            , index = index
            , enabled = Set.empty
            , used = Nothing
            }
        )


init : () -> ( App.Model, Cmd App.Msg )
init () =
    { currentThread = Nothing
    , addressbook =
        Dict.fromList
            (List.map (\entry -> ( entry.key, entry )) Example.addressBook)
    , you = "dawn"
    , context = {}
    , scripts = initScripts [ Example.exampleScript ]
    , inbox = []
    }
        |> C.with (Delay.after 500 Delay.Millisecond App.CheckForEnabled)



-- Update


update : App.Msg -> App.Model -> ( App.Model, Cmd App.Msg )
update msg model =
    case msg of
        App.ReturnToInbox threadIndex ->
            { model
                | currentThread = Nothing
                , inbox = updateInboxBecauseThisIndexHasBeenRead threadIndex model.inbox
            }
                |> C.with Cmd.none

        App.OpenThread threadIndex ->
            { model
                | currentThread = Just threadIndex
            }
                |> C.with Cmd.none

        App.MakeDecision index response ->
            { model | currentThread = Nothing, inbox = updateInboxBecauseThisIndexHasBeenRespondedTo index response.email model.inbox }
                |> C.with (Cmd.batch (List.map (App.DoAction index >> C.perform) response.actions))

        App.DoAction index action ->
            case action of
                Script.Enable str ->
                    { model
                        | scripts =
                            List.Extra.updateAt index
                                (\script -> { script | enabled = Set.insert str script.enabled })
                                model.scripts
                    }
                        |> C.with (Delay.after 2 Delay.Second App.CheckForEnabled)

        App.CheckForEnabled ->
            case Util.findSomething (findEnabledScene model.context) model.scripts of
                Nothing ->
                    model |> C.with Cmd.none

                Just ( updatedThreadScript, enabledScene ) ->
                    { model
                        | scripts =
                            List.Extra.updateAt updatedThreadScript.index
                                (\_ -> updatedThreadScript)
                                model.scripts
                        , inbox = updateInboxWithNewScene updatedThreadScript enabledScene model.inbox |> (\( x, xs ) -> x :: xs)
                    }
                        |> C.with Cmd.none


updateInboxBecauseThisIndexHasBeenRead : Int -> List App.ActiveThread -> List App.ActiveThread
updateInboxBecauseThisIndexHasBeenRead threadIndex =
    List.map
        (\thread ->
            if threadIndex == thread.index then
                case thread.state of
                    App.Unread responses ->
                        { thread | state = App.Unresponded responses }

                    _ ->
                        thread

            else
                thread
        )


updateInboxBecauseThisIndexHasBeenRespondedTo : Int -> Script.Email -> List App.ActiveThread -> List App.ActiveThread
updateInboxBecauseThisIndexHasBeenRespondedTo threadIndex newResponse =
    List.map
        (\thread ->
            if threadIndex == thread.index then
                { thread | state = App.Responded, contents = thread.contents ++ [ newResponse ] }

            else
                thread
        )


updateInboxWithNewScene : App.ThreadScript -> Script.ThreadScene -> List App.ActiveThread -> ( App.ActiveThread, List App.ActiveThread )
updateInboxWithNewScene threadScript threadScene inbox =
    case inbox of
        [] ->
            ( { index = threadScript.index
              , subject = threadScript.subject
              , people = []
              , contents = [ threadScene.receivedEmail ]
              , state = App.Unread threadScene.availableResponses
              }
            , []
            )

        thread :: threads ->
            if threadScript.index == thread.index then
                ( { thread
                    | contents = thread.contents ++ [ threadScene.receivedEmail ]
                    , state = App.Unread threadScene.availableResponses
                  }
                , threads
                )

            else
                let
                    ( updatedThread, otherThreads ) =
                        updateInboxWithNewScene threadScript threadScene threads
                in
                ( updatedThread, thread :: otherThreads )


sceneKeyEnabledInContext : App.ThreadScript -> Maybe String -> Maybe App.ThreadScript
sceneKeyEnabledInContext threadContext sceneKey =
    case ( sceneKey, threadContext.used ) of
        ( Nothing, Nothing ) ->
            Just { threadContext | used = Just Set.empty }

        -- Root emails are enabled if no root has been published
        ( Nothing, Just _ ) ->
            Nothing

        -- Root emails are disabled once a root is published
        ( Just _, Nothing ) ->
            Nothing

        -- No named emails are enabled until a root is published
        ( Just key, Just used ) ->
            if Set.member key threadContext.enabled && not (Set.member key used) then
                Just { threadContext | used = Just (Set.insert key used) }

            else
                Nothing


sceneEnabledInContext : App.GlobalContext -> App.ThreadScript -> Script.ThreadScene -> Maybe ( App.ThreadScript, Script.ThreadScene )
sceneEnabledInContext _ threadContext scene =
    sceneKeyEnabledInContext threadContext scene.key
        |> Maybe.andThen
            (\updatedThreadContext ->
                if List.length scene.guards == 0 then
                    Just ( updatedThreadContext, scene )

                else
                    -- TODO
                    Nothing
            )


findEnabledScene : App.GlobalContext -> App.ThreadScript -> Maybe ( App.ThreadScript, Script.ThreadScene )
findEnabledScene globalContext threadScript =
    Util.findSomething (sceneEnabledInContext globalContext threadScript) threadScript.scenes


-- Subscriptions


subscriptions : App.Model -> Sub App.Msg
subscriptions _ =
    Sub.none
