module Main2 exposing (main)

import Browser
import Cmd.Extra as C
import Dict exposing (Dict)
import Element exposing (Element, centerX, centerY, column, el, fill, height, padding, paragraph, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Example
import Html exposing (Html)
import Html.Attributes
import List.Extra
import Script
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { currentThread : Maybe ActiveThread
    , addressbook : Dict String Script.AddressbookEntry
    , you : String
    , script : List { enabled : Set String, used : Maybe (Set String), script : Script.ThreadScript }
    , inbox : List ActiveThread
    }


type alias ActiveThread =
    { index : Int
    , subject : String
    , people : List String
    , contents : List Script.Email
    , state : ActiveThreadState
    }


type ActiveThreadState
    = Unread (List Script.EmailResponse)
    | Unresponded (List Script.EmailResponse)
    | Responded


type Msg
    = ReturnToInbox Int
    | OpenThread ActiveThread
    | MakeDecision Int Script.EmailResponse
    | DoAction Int Script.Action
    | CheckForEnabled


init : () -> ( Model, Cmd Msg )
init () =
    { currentThread = Nothing
    , addressbook =
        Dict.fromList
            (List.map (\entry -> ( entry.key, entry )) Example.addressBook)
    , you = "dawn"
    , script = [ { enabled = Set.empty, used = Nothing, script = Example.exampleScript } ]
    , inbox = []
    }
        |> C.with (C.perform CheckForEnabled)



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReturnToInbox index ->
            { model | currentThread = Nothing, inbox = updateInboxItsRead index model.inbox } |> C.with Cmd.none

        OpenThread thread ->
            { model
                | currentThread = Just thread
            }
                |> C.with Cmd.none

        MakeDecision index response ->
            { model | currentThread = Nothing, inbox = updateInboxWithResponse index response.email model.inbox }
                |> C.with (Cmd.batch (List.map (DoAction index >> C.perform) response.actions))

        DoAction index action ->
            case action of
                Script.Enable str ->
                    { model
                        | script =
                            List.Extra.updateAt index
                                (\script -> { script | enabled = Set.insert str script.enabled })
                                model.script
                    }
                        |> C.with (C.perform CheckForEnabled)

        CheckForEnabled ->
            case findEnabledMessage 0 model.script of
                Nothing ->
                    model |> C.with Cmd.none

                Just ( index, component, script ) ->
                    { model
                        | script = script
                        , inbox = updateInboxWithNewIncoming index component model.inbox |> (\( x, xs ) -> x :: xs)
                    }
                        |> C.with Cmd.none


updateInboxItsRead : Int -> List ActiveThread -> List ActiveThread
updateInboxItsRead index =
    List.map
        (\thread ->
            if index == thread.index then
                case thread.state of
                    Unread responses ->
                        { thread | state = Unresponded responses }

                    _ ->
                        thread

            else
                thread
        )


updateInboxWithResponse : Int -> Script.Email -> List ActiveThread -> List ActiveThread
updateInboxWithResponse index email =
    List.map
        (\thread ->
            if index == thread.index then
                { thread | state = Responded, contents = thread.contents ++ [ email ] }

            else
                thread
        )


updateInboxWithNewIncoming : ( Int, Script.ThreadScript ) -> Script.ScriptComponent -> List ActiveThread -> ( ActiveThread, List ActiveThread )
updateInboxWithNewIncoming ( index, script ) component inbox =
    case inbox of
        [] ->
            ( { index = index
              , subject = script.subject
              , people = []
              , contents = [ component.receivedEmail ]
              , state = Unread component.availableResponses
              }
            , []
            )

        thread :: threads ->
            if index == thread.index then
                ( { thread
                    | contents = thread.contents ++ [ component.receivedEmail ]
                    , state = Unread component.availableResponses
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
    -> List { enabled : Set String, used : Maybe (Set String), script : Script.ThreadScript }
    -> Maybe ( ( Int, Script.ThreadScript ), Script.ScriptComponent, List { enabled : Set String, used : Maybe (Set String), script : Script.ThreadScript } )
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


importantYes : Svg msg
importantYes =
    Svg.svg [ Html.Attributes.width 24, Html.Attributes.height 10 ]
        [ Svg.polygon
            [ Svg.Attributes.fill "yellow"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.points "0,0 13,0 18,5 13,10 0,10 5,5"
            ]
            []
        ]


importantNo : Svg msg
importantNo =
    Svg.svg [ Html.Attributes.width 24, Html.Attributes.height 10 ]
        [ Svg.polygon
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke "gray"
            , Svg.Attributes.points "0,0 13,0 18,5 13,10 0,10 5,5"
            ]
            []
        ]


starNo : Svg msg
starNo =
    Svg.svg [ Html.Attributes.width 20, Html.Attributes.height 19 ]
        [ Svg.polygon
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke "gray"
            , Svg.Attributes.points "4,19 4,12 1,7 7,5 10,0 13,5 19,7 16,12 16,19 10,16.5"
            ]
            []
        ]


view : Model -> Html Msg
view model =
    Element.layout
        [ height fill
        , width fill
        ]
        (browserUI model)


browserUI : Model -> Element Msg
browserUI model =
    row
        [ width fill
        , height fill
        , spacing 0
        ]
        [ leftBar
        , mainPanel model
        ]


leftBar : Element msg
leftBar =
    el
        [ width (px 250)
        , height fill
        ]
        (column
            [ width fill
            , height fill
            , spacing 20
            ]
            [ el [] Element.none
            , el [ Element.centerX ] (text "Gabble Mail")
            ]
        )


viewInbox : List ActiveThread -> Element Msg
viewInbox threads =
    column
        [ Background.color (rgb255 200 200 200)
        , spacing 1
        , width fill
        , height fill
        ]
        (List.map viewThreadPreview threads)


threadHeight : Element.Length
threadHeight =
    px 35


viewThreadPreview : ActiveThread -> Element Msg
viewThreadPreview thread =
    let
        ( weight, bgColor, important ) =
            case thread.state of
                Unread _ ->
                    ( Font.bold, rgb255 255 255 255, importantYes )

                Unresponded _ ->
                    ( Font.regular, rgb255 240 240 240, importantYes )

                Responded ->
                    ( Font.regular, rgb255 240 240 240, importantNo )
    in
    row
        [ width fill, height threadHeight, Background.color bgColor ]
        [ el [ width threadHeight, centerY ] (el [ centerX ] (Element.html starNo))
        , el [ width threadHeight, centerY ] (el [ centerX ] (Element.html important))
        , el
            [ weight, width (px 250), height fill, Element.pointer, Events.onClick (OpenThread thread) ]
            (el [ centerY ] (text "(todo: participants)"))
        , el
            [ weight, width fill, height fill, Element.pointer, Events.onClick (OpenThread thread) ]
            (el [ centerY ] (text thread.subject))
        , el
            [ weight, width (px 150), height fill, Element.alignRight, Element.pointer, Events.onClick (OpenThread thread) ]
            (el [ centerY, Element.alignRight ] (text "1:15 PM"))
        , el [ width threadHeight, height threadHeight, Element.alignRight ] Element.none
        ]


viewThread : ActiveThread -> Element Msg
viewThread thread =
    row [ width fill, height fill ]
        [ el [ width threadHeight, height threadHeight ] Element.none
        , el [ width threadHeight, height threadHeight ] Element.none
        , column [ width fill, height fill, spacing 10 ]
            (el [ height (px 10) ] Element.none
                :: el [ Font.size 30 ] (text thread.subject)
                :: el [ height (px 10) ] Element.none
                :: List.map (\{ from, to, contents } -> paragraph [] (List.map text contents)) thread.contents
                ++ [ case thread.state of
                        Responded ->
                            Element.none

                        Unread options ->
                            row [ width fill, spacing 10 ]
                                (List.map
                                    (\responseOption ->
                                        el [ padding 10, Background.color (rgb255 200 200 200), Events.onClick (MakeDecision thread.index responseOption) ]
                                            (text responseOption.shortText)
                                    )
                                    options
                                )

                        Unresponded options ->
                            row [ width fill, spacing 10 ]
                                (List.map
                                    (\responseOption ->
                                        el [ padding 10, Background.color (rgb255 200 200 200), Events.onClick (MakeDecision thread.index responseOption) ]
                                            (text responseOption.shortText)
                                    )
                                    options
                                )
                   ]
            )
        , el [ width threadHeight, height threadHeight ] Element.none
        ]


mainPanel : Model -> Element Msg
mainPanel model =
    column
        [ height fill
        , width fill
        ]
        [ el
            [ width fill
            , height threadHeight
            ]
            Element.none
        , el
            [ width threadHeight
            , height threadHeight
            ]
            (case model.currentThread of
                Nothing ->
                    Element.none

                Just activeThread ->
                    el [ Events.onClick (ReturnToInbox activeThread.index), centerX, centerY, Element.pointer ] (text "<-")
            )
        , el [ width fill, height (px 1), Background.color (rgb255 200 200 200) ] Element.none
        , el
            [ width fill
            , height fill
            , Element.scrollbarY
            ]
            (case model.currentThread of
                Nothing ->
                    viewInbox model.inbox

                Just thread ->
                    viewThread thread
            )
        ]
