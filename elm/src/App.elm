module App exposing (main)

import Assets
import Browser
import Cmd.Extra as Cmd
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html exposing (Html)
import List.Extra as List
import Script as S
import ScriptTypes as Script
import Set
import Util


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { blocked : Maybe { scriptId : String, next : String }
    , scripts : List Script.ThreadScript
    , inbox : List ActiveThread
    , state : InboxState
    }


type InboxState
    = ThreadOpen { location : ThreadLocation }
    | InboxOpen


type alias ThreadLocation =
    { inboxIndex : Int
    , scriptId : String
    }


type alias ActiveThread =
    { scriptId : String
    , contents : List Script.Email
    , state : ActiveThreadState
    }


type ActiveThreadState
    = Responded { archivable : Bool }
    | Archived
    | Unread { archivable : Bool, responseOptions : List Script.EmailResponse }
    | Unresponded
        { archivable : Bool
        , responseOptions : List Script.EmailResponse
        , currentlySelectedOptionIndex : Maybe Int
        }


init : () -> ( Model, Cmd Msg )
init () =
    { state = InboxOpen
    , blocked = Nothing
    , inbox =
        S.myScript
            |> List.map
                (\{ id, first, actions } ->
                    { scriptId = id
                    , contents = [ first ]
                    , state = Unread { archivable = False, responseOptions = actions }
                    }
                )
    , scripts = S.myScript
    }
        |> Cmd.pure



-- Update


type Msg
    = ReturnToInbox
    | OpenThread ThreadLocation
    | ToggleSuggestion Int
    | SelectSuggestion
    | ArchiveThread


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReturnToInbox ->
            { model | state = InboxOpen }
                |> Cmd.pure

        OpenThread location ->
            let
                inboxWithRead =
                    -- Mark current thread as unread
                    List.map
                        (\thread ->
                            if thread.scriptId /= location.scriptId then
                                thread

                            else
                                case thread.state of
                                    Unread { archivable, responseOptions } ->
                                        { thread
                                            | state =
                                                case responseOptions of
                                                    [] ->
                                                        Responded { archivable = archivable }

                                                    _ ->
                                                        Unresponded
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
                , state = ThreadOpen { location = location }
            }
                |> Cmd.pure

        ToggleSuggestion n ->
            case model.state of
                ThreadOpen { location } ->
                    { model
                        | inbox =
                            List.indexedMap
                                (\inboxIndex thread ->
                                    if inboxIndex /= location.inboxIndex then
                                        thread

                                    else
                                        case thread.state of
                                            Unresponded state ->
                                                if state.currentlySelectedOptionIndex == Just n then
                                                    { thread | state = Unresponded { state | currentlySelectedOptionIndex = Nothing } }

                                                else
                                                    { thread | state = Unresponded { state | currentlySelectedOptionIndex = Just n } }

                                            _ ->
                                                thread
                                )
                                model.inbox
                    }
                        |> Cmd.pure

                _ ->
                    -- Should be impossible!
                    model |> Cmd.pure

        ArchiveThread ->
            case model.state of
                ThreadOpen { location } ->
                    { model
                        | inbox =
                            model.inbox
                                |> List.updateAt location.inboxIndex
                                    (\thread -> { thread | state = Archived })
                                |> advanceInbox model
                        , state = InboxOpen
                    }
                        |> Cmd.pure

                _ ->
                    -- Should be impossible!
                    model |> Cmd.pure

        SelectSuggestion ->
            case model.state of
                ThreadOpen { location } ->
                    let
                        thread =
                            getThread location.inboxIndex model

                        ( threadWithResponse, blockedNext ) =
                            -- Append the selected email to the end of its thread,
                            -- change that thread's state to Responded
                            case thread.state of
                                Unresponded { responseOptions, currentlySelectedOptionIndex } ->
                                    case currentlySelectedOptionIndex of
                                        Nothing ->
                                            -- Should be impossible!
                                            ( thread, Nothing )

                                        Just index ->
                                            case List.getAt index responseOptions of
                                                Nothing ->
                                                    -- Should be impossible!
                                                    ( thread, Nothing )

                                                Just response ->
                                                    ( { thread
                                                        | contents = thread.contents ++ [ response.email ]
                                                        , state = Responded { archivable = False }
                                                      }
                                                    , Just response.next
                                                    )

                                _ ->
                                    -- Should be impossible!
                                    ( thread, Nothing )
                    in
                    { model
                        | inbox =
                            model.inbox
                                -- Replace the appropriate thread with the
                                -- updated thread
                                |> List.setAt location.inboxIndex threadWithResponse
                                |> advanceInbox model
                        , blocked = blockedNext |> Maybe.map (\blockedNextId -> { scriptId = location.scriptId, next = blockedNextId })
                        , state = InboxOpen
                    }
                        |> Cmd.pure

                _ ->
                    -- Should be impossible!
                    model |> Cmd.pure


advanceInbox : Model -> List ActiveThread -> List ActiveThread
advanceInbox model inbox =
    case model.blocked of
        Nothing ->
            inbox

        Just { scriptId, next } ->
            case List.splitWhen (\thread -> thread.scriptId == scriptId) inbox of
                Just ( prefix, thread :: postfix ) ->
                    case advanceThread model scriptId next of
                        ( [], _, _ ) ->
                            -- Nothing matching `next` was found in the script for this thread
                            -- This could be made impossible, but for now just represents a finished script
                            inbox

                        ( newEmails, responseOptions, archivable ) ->
                            { thread
                                | contents = thread.contents ++ newEmails
                                , state = Unread { archivable = archivable, responseOptions = responseOptions }
                            }
                                :: prefix
                                ++ postfix

                _ ->
                    -- Should be impossible! (scriptId not found in inbox)
                    inbox


advanceThread : Model -> String -> String -> ( List Script.Email, List Script.EmailResponse, Bool )
advanceThread model scriptId next =
    let
        script =
            getScript scriptId model
    in
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
                                advanceThread model scriptId goto
                        in
                        ( receivedEmail :: newEmails, action, archivable )

                    Nothing ->
                        -- Base case: this is the last email in the thread
                        -- extension
                        ( [ receivedEmail ], responseOptions, List.any ((==) Script.Archive) actions )
            )
        |> Maybe.withDefault ( [], [], False )



-- { thread | contents = thread.contents ++ [ receivedEmail ] })
-- View


threadHeight : Element.Length
threadHeight =
    px 35


leftBuffer : Element.Length
leftBuffer =
    px 70


leftBuffer1 : Element.Length
leftBuffer1 =
    px 35


leftBuffer2 : Element.Length
leftBuffer2 =
    px 35


buttonSpacing : Int
buttonSpacing =
    20


rightBuffer : Element.Length
rightBuffer =
    px 35


dimmedText : Color
dimmedText =
    rgb255 120 120 120


uiGray : Color
uiGray =
    rgb255 200 200 200


suggestionColor : Color
suggestionColor =
    rgb255 50 130 255


responseSeparator : Length
responseSeparator =
    px 20


separator : Element msg
separator =
    el [ height (px 1), Background.color uiGray, width fill ] none


view : Model -> Html Msg
view model =
    Element.layout [ height fill, width fill ] <|
        row [ width fill, height fill, spacing 0 ] <|
            [ -- Left Bar
              el [ width <| px 250, height fill ] <|
                (column [ width fill, height fill, spacing 5 ] <|
                    [ el [ height <| px 45 ] Element.none
                    , el [ Element.centerX ] (text "Camperdown")
                    , el [ Element.centerX ] (text "Email Client")
                    ]
                )
            , -- Main panel
              column [ height fill, width fill ] <|
                [ el [ width fill, height threadHeight ] <|
                    none
                , el [ width threadHeight, height threadHeight ] <|
                    toolbar model
                , el [ width fill, height (px 1), Background.color (rgb255 200 200 200) ] <|
                    none
                , el [ width fill, height fill, scrollbarY ] <|
                    case model.state of
                        InboxOpen ->
                            inboxFull model

                        ThreadOpen { location } ->
                            threadFull model location
                ]
            ]


toolbar : Model -> Element Msg
toolbar model =
    case model.state of
        InboxOpen ->
            none

        ThreadOpen { location } ->
            let
                thread =
                    getThread location.inboxIndex model

                archive canArchive =
                    if canArchive then
                        el [ Events.onClick ArchiveThread, centerX, centerY, pointer ] (text "↓")

                    else
                        el [ centerX, centerY, Font.color dimmedText ] (text "↓")
            in
            row [ spacing 10 ]
                [ el [ Events.onClick ReturnToInbox, centerX, centerY, pointer ] (text "←")
                , case thread.state of
                    Unresponded { archivable } ->
                        archive archivable

                    _ ->
                        archive False
                ]


inboxFull : Model -> Element Msg
inboxFull model =
    column [ Background.color (rgb255 200 200 200), spacing 1, width fill, height fill ] <|
        (model.inbox
            |> List.indexedMap (threadPreview model)
            |> List.filterMap identity
        )


getScript : String -> Model -> Script.ThreadScript
getScript needle model =
    List.find (\script -> needle == script.id) model.scripts
        |> Maybe.withDefault
            { id = needle
            , subject = "Error, can't find " ++ needle
            , scenes = Dict.empty
            , first = { from = S.me, to = [ S.me ], contents = [] }
            , actions = []
            }


getThread : Int -> Model -> ActiveThread
getThread index model =
    List.getAt index model.inbox
        |> Maybe.withDefault
            { scriptId = String.fromInt index
            , contents = []
            , state = Responded { archivable = False }
            }


threadPreview : Model -> Int -> ActiveThread -> Maybe (Element Msg)
threadPreview model inboxIndex { scriptId, contents, state } =
    let
        ( weight, bgColor, important ) =
            case state of
                Unread _ ->
                    ( Font.bold, rgb255 255 255 255, Assets.importantYes )

                Unresponded _ ->
                    ( Font.regular, rgb255 240 240 240, Assets.importantYes )

                Responded _ ->
                    ( Font.regular, rgb255 240 240 240, Assets.importantNo )

                Archived ->
                    -- Doesn't matter
                    ( Font.regular, rgb255 240 240 240, Assets.importantNo )

        location =
            { scriptId = scriptId, inboxIndex = inboxIndex }
    in
    if state == Archived then
        Nothing

    else
        row
            [ width fill, height threadHeight, Background.color bgColor ]
            [ el [ width leftBuffer1, centerY ] (el [ centerX ] (Element.html Assets.starNo))
            , el [ width leftBuffer2, centerY ] (el [ centerX ] (Element.html important))
            , el
                [ weight, width (px 250), height fill, Element.pointer, Events.onClick (OpenThread location) ]
                (el [ centerY ] (text "People"))
            , el
                [ weight, width fill, height fill, Element.pointer, Events.onClick (OpenThread location) ]
                (el [ centerY ] (getScript scriptId model |> .subject |> text))
            , el
                [ weight, width (px 150), height fill, Element.alignRight, Element.pointer, Events.onClick (OpenThread location) ]
                (el [ centerY, Element.alignRight ] (text (String.fromInt (String.length scriptId) ++ " KB")))
            , el [ width rightBuffer, height threadHeight, Element.alignRight ] Element.none
            ]
            |> Just


threadFull : Model -> ThreadLocation -> Element Msg
threadFull model ({ inboxIndex, scriptId } as loc) =
    let
        script =
            getScript scriptId model

        thread =
            getThread inboxIndex model
    in
    column [ width fill, height fill, spacing 20 ]
        (el [ height (px 10) ] Element.none
            :: row [ width fill ]
                [ el [ width leftBuffer ] none
                , el [ Font.size 24 ] (text script.subject)
                ]
            :: (List.map viewEmail thread.contents |> List.intersperse separator)
            ++ [ case thread.state of
                    Unresponded { responseOptions, currentlySelectedOptionIndex } ->
                        suggestionPicker responseOptions currentlySelectedOptionIndex

                    _ ->
                        none
               ]
        )


suggestionButton : Bool -> Int -> String -> Element Msg
suggestionButton selected suggestionIndex shortMessage =
    let
        ( fontColor, backgroundColor ) =
            if selected then
                ( rgb255 255 255 255, suggestionColor )

            else
                ( suggestionColor, rgb255 255 255 255 )
    in
    el
        [ Font.color fontColor
        , Background.color backgroundColor
        , Events.onClick (ToggleSuggestion suggestionIndex)
        , Border.color suggestionColor
        , Border.solid
        , Border.width 1
        , Border.rounded 5
        , paddingXY 20 10
        , width shrink
        ]
        (text shortMessage)


suggestionPicker : List Script.EmailResponse -> Maybe Int -> Element Msg
suggestionPicker responseOptions currentlySelectedOptionIndex =
    column [ width fill ]
        [ -- Selections
          row [ width fill ]
            [ el [ width leftBuffer ] none
            , wrappedRow [ spacing buttonSpacing, width fill ]
                (List.indexedMap
                    (\suggestionIndex responseOption ->
                        suggestionButton
                            (currentlySelectedOptionIndex == Just suggestionIndex)
                            suggestionIndex
                            responseOption.shortText
                    )
                    responseOptions
                )
            ]
        , -- Contents of selected email
          List.getAt (currentlySelectedOptionIndex |> Maybe.withDefault -1) responseOptions
            |> Maybe.map viewEmailResponse
            |> Maybe.withDefault none
        ]


viewEmailResponse : Script.EmailResponse -> Element Msg
viewEmailResponse emailResponse =
    column [ width fill ]
        [ el [ height responseSeparator ] none
        , row [ width fill ]
            [ el [ width leftBuffer, centerX, alignTop ] (html Assets.idCircle)
            , el
                -- [
                [ Border.solid
                , Border.rounded 5
                , padding 15
                , width fill
                , Border.glow uiGray 1.0
                ]
                (column [ width fill, spacing 20 ]
                    [ viewResponse "To" emailResponse.email.to
                    , column [ spacing 10 ] (List.map (\par -> paragraph [] [ text par ]) emailResponse.email.contents)
                    , separator
                    , el
                        [ Events.onClick SelectSuggestion
                        , Font.color (rgb255 255 255 255)
                        , Background.color suggestionColor
                        , Border.color suggestionColor
                        , Border.solid
                        , Border.width 1
                        , Border.rounded 5
                        , paddingXY 20 10
                        , width shrink
                        ]
                        (text "Send")
                    ]
                )
            , el [ width rightBuffer ] none
            ]
        ]


viewResponse : String -> List Script.AddressbookEntry -> Element msg
viewResponse kind records =
    case records of
        [] ->
            none

        _ ->
            row [ width fill, spacing 15 ] [ text kind, wrappedRow [ width fill, spacing 15 ] (List.map toPill records) ]


toPill : Script.AddressbookEntry -> Element msg
toPill record =
    el [ paddingXY 10 0, height (px 22), Border.width 1, Border.rounded 10, Font.size 15, Border.color (rgb255 255 140 0) ] (el [ centerY ] (text record.full))


viewEmail : Script.Email -> Element Msg
viewEmail email =
    let
        to =
            email.to
                |> List.map (\x -> x.full)
                |> List.intersperse ", "
                |> String.concat
    in
    row [ width fill ]
        [ el [ width leftBuffer, centerX, alignTop ] (html Assets.idCircle)
        , column [ width fill, spacing 10 ]
            (paragraph [ Font.size 15 ]
                [ el [ Font.bold ] (text email.from.full)
                , el [ Font.color dimmedText ] (text ("  <" ++ email.from.email ++ ">"))
                ]
                :: paragraph [ Font.size 15, Font.color dimmedText ] [ text ("to " ++ to) ]
                :: List.map (text >> List.singleton >> paragraph []) email.contents
            )
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
