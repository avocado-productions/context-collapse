module View exposing (view)

import AppTypes as App exposing (Msg(..))
import Assets
import Color
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
import List.Extra as List
import ScriptTypes as Script
import UI


view : App.Model -> Html App.Msg
view model =
    Element.layout [ height fill, width fill ] <|
        row [ width fill, height fill, spacing 0 ] <|
            [ -- Left Bar
              row [ paddingEach { top = UI.externalChromePadding, left = UI.externalChromePadding, right = 0, bottom = 0 }, width <| px UI.leftMenuWidth, height fill ]
                [ image [ width <| px UI.logoWidth, height <| px UI.logoHeight, alignTop ] { src = "assets/avocomm-logo.png", description = "AvoComm webmail client logo" }
                , el [ width fill, height <| px UI.logoHeight, alignTop, Font.size 54, UI.uiFont ] (el [ alignBottom ] <| text "voComm")
                ]
            , -- Main panel
              column [ height fill, width fill ] <|
                [ el [ width fill, height UI.threadHeight ] <|
                    none
                , el [ width UI.threadHeight, height UI.threadHeight ] <|
                    toolbar model
                , el [ width fill, height (px 10) ] <|
                    none
                , el [ width fill, height (px 1), Background.color Color.uiGray ] <|
                    none
                , el [ width fill, height fill, scrollbarY ] <|
                    case model.state of
                        App.InboxOpen ->
                            inboxFull model

                        App.ThreadOpen { location } ->
                            threadFull model location
                ]
            ]


toolbarButton symbol desc action =
    row
        ([ spacing 4, height (px 40), paddingXY 10 0, Border.rounded 40, Region.navigation, htmlAttribute (Html.Attributes.style "user-select" "none") ]
            ++ (case action of
                    Just msg ->
                        [ Events.onClick msg, pointer, mouseOver [ Background.color Color.uiLightGray ] ]

                    Nothing ->
                        [ Font.color Color.dimmedText ]
               )
        )
        [ el [ centerY, Font.size 20, alignTop, centerY ] (text symbol)
        , el [ centerY, UI.uiFont, Font.hairline, Font.size 20, centerY, paddingEach { top = 4, bottom = 0, left = 0, right = 0 } ] (text desc)
        ]


toolbar : App.Model -> Element App.Msg
toolbar model =
    case model.state of
        App.InboxOpen ->
            row []
                [ toolbarButton "⟳" "Refresh" (Just App.ReturnToInbox) ]

        App.ThreadOpen { location } ->
            let
                thread =
                    getThread location.inboxIndex model

                archive canArchive =
                    if canArchive then
                        toolbarButton "↓" "Archive" (Just App.ArchiveThread)

                    else
                        toolbarButton "↓" "Archive" Nothing
            in
            row []
                [ toolbarButton "←" "Return to Inbox" (Just App.ReturnToInbox)
                , case thread.state of
                    App.Unresponded { archivable } ->
                        archive archivable

                    App.Responded { archivable } ->
                        archive archivable

                    _ ->
                        archive False
                ]


inboxFull : App.Model -> Element App.Msg
inboxFull model =
    if List.all (\{ state } -> state == App.Archived) model.inbox then
        el [ Background.color Color.uiGray, width fill, height fill ] <|
            column [ centerY, spacing 20, width fill ]
                [ el [ centerX, Font.color Color.dimmedText ] <| text "You're all done!"
                , el [ centerX, Font.color Color.dimmedText, Font.size 10 ] <| text "Nothing in Inbox"
                ]

    else
        column [ Background.color Color.uiGray, spacing 1, width fill, height fill ] <|
            (model.inbox
                |> List.indexedMap (threadPreview model)
                |> List.filterMap identity
            )


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


threadPreview : App.Model -> Int -> App.ActiveThread -> Maybe (Element App.Msg)
threadPreview model inboxIndex { scriptId, contents, state } =
    let
        ( weight, bgColor, important ) =
            case state of
                App.Unread _ ->
                    ( Font.bold, Color.white, Assets.importantYes )

                App.Unresponded _ ->
                    ( Font.regular, Color.uiLightGray, Assets.importantYes )

                App.Responded _ ->
                    ( Font.regular, Color.uiLightGray, Assets.importantNo )

                App.Archived ->
                    -- Doesn't matter
                    ( Font.regular, Color.uiLightGray, Assets.importantNo )

        location =
            { scriptId = scriptId, inboxIndex = inboxIndex }
    in
    if state == App.Archived then
        Nothing

    else
        row
            [ width fill, height UI.threadHeight, Background.color bgColor ]
            [ el [ width UI.leftBuffer1, centerY ] (el [ centerX ] (Element.html Assets.starNo))
            , el [ width UI.leftBuffer2, centerY ] (el [ centerX ] (Element.html important))
            , el
                [ weight, width (px 250), height fill, Element.pointer, Events.onClick (App.OpenThread location) ]
                (el [ centerY ] (text "People"))
            , el
                [ weight, width fill, height fill, Element.pointer, Events.onClick (App.OpenThread location) ]
                (el [ centerY ] (getScript scriptId model |> .subject |> text))
            , el
                [ weight, width (px 150), height fill, Element.alignRight, Element.pointer, Events.onClick (App.OpenThread location) ]
                (el [ centerY, Element.alignRight ] (text (String.fromInt (String.length scriptId) ++ " KB")))
            , el [ width UI.rightBuffer, height UI.threadHeight, Element.alignRight ] Element.none
            ]
            |> Just


threadFull : App.Model -> App.ThreadLocation -> Element App.Msg
threadFull model { inboxIndex, scriptId } =
    let
        script =
            getScript scriptId model

        thread =
            getThread inboxIndex model
    in
    column [ width fill, height fill, spacing 20 ]
        (el [ height (px 10) ] Element.none
            :: row [ width fill ]
                [ el [ width UI.leftBuffer ] none
                , el [ Font.size 24 ] (text script.subject)
                ]
            :: (List.map viewEmail thread.contents |> List.intersperse UI.separator)
            ++ [ case thread.state of
                    App.Unresponded { responseOptions, currentlySelectedOptionIndex } ->
                        suggestionPicker responseOptions currentlySelectedOptionIndex

                    _ ->
                        none
               ]
        )


suggestionButton : Bool -> Int -> String -> Element App.Msg
suggestionButton selected suggestionIndex shortMessage =
    let
        ( fontColor, backgroundColor ) =
            if selected then
                ( Color.white, Color.suggestionColor )

            else
                ( Color.suggestionColor, Color.white )
    in
    el
        [ Font.color fontColor
        , Background.color backgroundColor
        , Events.onClick (App.ToggleSuggestion suggestionIndex)
        , Border.color Color.suggestionColor
        , Border.solid
        , Border.width 1
        , Border.rounded 5
        , paddingXY 20 10
        , width shrink
        ]
        (text shortMessage)


suggestionPicker : List Script.EmailResponse -> Maybe Int -> Element App.Msg
suggestionPicker responseOptions currentlySelectedOptionIndex =
    column [ width fill ]
        [ -- Selections
          row [ width fill ]
            [ el [ width UI.leftBuffer ] none
            , wrappedRow [ spacing UI.buttonSpacing, width fill ]
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


viewEmailResponse : Script.EmailResponse -> Element App.Msg
viewEmailResponse emailResponse =
    column [ width fill ]
        [ el [ height UI.responseSeparator ] none
        , row [ width fill ]
            [ el [ width UI.leftBuffer, centerX, alignTop ] (html Assets.idCircle)
            , el
                -- [
                [ Border.solid
                , Border.rounded 5
                , padding 15
                , width fill
                , Border.glow Color.uiGray 1.0
                ]
                (column [ width fill, spacing 20 ]
                    [ viewResponse "To" emailResponse.email.to
                    , column [ spacing 10 ] (List.map (\par -> paragraph [] [ text par ]) emailResponse.email.contents)
                    , UI.separator
                    , el
                        [ Events.onClick App.SelectSuggestion
                        , Font.color Color.white
                        , Background.color Color.suggestionColor
                        , Border.color Color.suggestionColor
                        , Border.solid
                        , Border.width 1
                        , Border.rounded 5
                        , paddingXY 20 10
                        , width shrink
                        ]
                        (text "Send")
                    ]
                )
            , el [ width UI.rightBuffer ] none
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


viewEmail : Script.Email -> Element App.Msg
viewEmail email =
    let
        to =
            email.to
                |> List.map (\x -> x.full)
                |> List.intersperse ", "
                |> String.concat
    in
    row [ width fill ]
        [ el [ width UI.leftBuffer, centerX, alignTop ] (html Assets.idCircle)
        , column [ width fill, spacing 10 ]
            (paragraph [ Font.size 15 ]
                [ el [ Font.bold ] (text email.from.full)
                , el [ Font.color Color.dimmedText ] (text ("  <" ++ email.from.email ++ ">"))
                ]
                :: paragraph [ Font.size 15, Font.color Color.dimmedText ] [ text ("to " ++ to) ]
                :: List.map (text >> List.singleton >> paragraph []) email.contents
            )
        , el [ width UI.rightBuffer ] none
        ]
