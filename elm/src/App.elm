module App exposing (main)

import AppTypes as App
import Browser
import Cmd.Extra as C
import Dict exposing (Dict)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Example
import Html exposing (Html)
import Html.Attributes
import List.Extra
import ScriptTypes as Script
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes
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
    { currentThread = Nothing
    , addressbook =
        Dict.fromList
            (List.map (\entry -> ( entry.key, entry )) Example.addressBook)
    , you = "dawn"
    , script = [ { enabled = Set.empty, used = Nothing, script = Example.exampleScript } ]
    , inbox = []
    }
        |> C.with (C.perform App.CheckForEnabled)



-- Update


update : App.Msg -> App.Model -> ( App.Model, Cmd App.Msg )
update msg model =
    case msg of
        App.ReturnToInbox index ->
            { model | currentThread = Nothing, inbox = updateInboxItsRead index model.inbox } |> C.with Cmd.none

        App.OpenThread thread ->
            { model
                | currentThread = Just thread
            }
                |> C.with Cmd.none

        App.MakeDecision index response ->
            { model | currentThread = Nothing, inbox = updateInboxWithResponse index response.email model.inbox }
                |> C.with (Cmd.batch (List.map (App.DoAction index >> C.perform) response.actions))

        App.DoAction index action ->
            case action of
                Script.Enable str ->
                    { model
                        | script =
                            List.Extra.updateAt index
                                (\script -> { script | enabled = Set.insert str script.enabled })
                                model.script
                    }
                        |> C.with (C.perform App.CheckForEnabled)

        App.CheckForEnabled ->
            case findEnabledMessage 0 model.script of
                Nothing ->
                    model |> C.with Cmd.none

                Just ( index, component, script ) ->
                    { model
                        | script = script
                        , inbox = updateInboxWithNewIncoming index component model.inbox |> (\( x, xs ) -> x :: xs)
                    }
                        |> C.with Cmd.none


updateInboxItsRead : Int -> List App.ActiveThread -> List App.ActiveThread
updateInboxItsRead index =
    List.map
        (\thread ->
            if index == thread.index then
                case thread.state of
                    App.Unread responses ->
                        { thread | state = App.Unresponded responses }

                    _ ->
                        thread

            else
                thread
        )


updateInboxWithResponse : Int -> Script.Email -> List App.ActiveThread -> List App.ActiveThread
updateInboxWithResponse index email =
    List.map
        (\thread ->
            if index == thread.index then
                { thread | state = App.Responded, contents = thread.contents ++ [ email ] }

            else
                thread
        )


updateInboxWithNewIncoming : ( Int, Script.ThreadScript ) -> Script.ScriptComponent -> List App.ActiveThread -> ( App.ActiveThread, List App.ActiveThread )
updateInboxWithNewIncoming ( index, script ) component inbox =
    case inbox of
        [] ->
            ( { index = index
              , subject = script.subject
              , people = []
              , contents = [ component.receivedEmail ]
              , state = App.Unread component.availableResponses
              }
            , []
            )

        thread :: threads ->
            if index == thread.index then
                ( { thread
                    | contents = thread.contents ++ [ component.receivedEmail ]
                    , state = App.Unread component.availableResponses
                  }
                , threads
                )

            else
                let
                    ( updatedThread, otherThreads ) =
                        updateInboxWithNewIncoming ( index, script ) component threads
                in
                ( updatedThread, thread :: otherThreads )


keyEnabled key enabled used =
    case ( key, used ) of
        ( Nothing, Nothing ) ->
            Just Set.empty

        -- Root emails are enabled if no root has been published
        ( Nothing, Just _ ) ->
            Nothing

        -- Root emails are disabled once a root is published
        ( Just _, Nothing ) ->
            Nothing

        -- No named emails are enabled until a root is published
        ( Just k, Just u ) ->
            if Set.member k enabled && not (Set.member k u) then
                Just (Set.insert k u)

            else
                Nothing


checkScriptForEnabledMessage : Set String -> Maybe (Set String) -> List Script.Step -> Maybe ( Script.ScriptComponent, Set String )
checkScriptForEnabledMessage enabled used script =
    case script of
        [] ->
            Nothing

        step :: steps ->
            case keyEnabled step.key enabled used of
                Nothing ->
                    checkScriptForEnabledMessage enabled used steps

                Just newUsed ->
                    --- TODO ADD CONDITIONS HERE
                    if List.length step.guards == 0 then
                        Just ( step.contents, newUsed )

                    else
                        checkScriptForEnabledMessage enabled used steps


findEnabledMessage :
    Int
    -> List App.ScriptWithContext
    -> Maybe ( ( Int, Script.ThreadScript ), Script.ScriptComponent, List App.ScriptWithContext )
findEnabledMessage index threadScripts =
    case threadScripts of
        [] ->
            Nothing

        script :: remainingThreadScripts ->
            case checkScriptForEnabledMessage script.enabled script.used script.script.script of
                Nothing ->
                    findEnabledMessage (index + 1) remainingThreadScripts
                        |> Maybe.map (\( i, component, modifiedScripts ) -> ( i, component, script :: modifiedScripts ))

                Just ( component, used ) ->
                    Just ( ( index, script.script ), component, { script | used = Just used } :: remainingThreadScripts )



-- Subscriptions


subscriptions : App.Model -> Sub App.Msg
subscriptions _ =
    Sub.none
