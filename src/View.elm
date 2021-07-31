module View exposing (view)

import App as App exposing (Msg(..))
import Assets
import Browser exposing (Document)
import Color
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Region as Region
import Html.Attributes
import List.Extra as List
import ScriptTypes as Script
import UI
import Util


view : App.Model -> Document App.Msg
view model =
    { title =
        case model.state of
            App.InboxOpen ->
                let
                    unread =
                        model.inbox
                            |> List.filterMap
                                (\email ->
                                    case email.state of
                                        App.Unread _ ->
                                            Just ()

                                        _ ->
                                            Nothing
                                )
                            |> List.length
                in
                if unread == 0 then
                    "Inbox - " ++ model.me.email ++ " - AvoComm"

                else
                    "Inbox (" ++ String.fromInt unread ++ ") - " ++ model.me.email ++ " - AvoComm"

            App.ThreadOpen { location } ->
                let
                    script =
                        getScript location.scriptId model
                in
                script.subject ++ " - " ++ model.me.email ++ " - AvoComm"
    , body =
        [ Element.layout [ height fill, width fill ] <|
            row [ width fill, height fill, spacing 0 ] <|
                [ -- Left Bar
                  row [ paddingEach { top = UI.externalChromePadding, left = UI.externalChromePadding, right = 0, bottom = 0 }, width <| px UI.leftMenuWidth, height fill ]
                    [ image [ width <| px UI.logoWidth, height <| px UI.logoHeight, alignTop ] { src = "assets/avocomm-logo.png", description = "AvoComm webmail client logo" }
                    , el [ width fill, height <| px UI.logoHeight, alignTop, Font.size 54, UI.logoFont ] (el [ alignBottom ] <| text "voComm")
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
        ]
    }


toolbarButton : Bool -> String -> String -> Maybe msg -> Element msg
toolbarButton emphasize symbol desc action =
    let
        extraBorder =
            if emphasize then
                [ Border.color Color.suggestionColor
                , Border.solid
                , Border.width 1
                ]

            else
                []
    in
    row
        ([ spacing 4, height (px 40), paddingXY 10 0, Border.rounded 40, Region.navigation, htmlAttribute (Html.Attributes.style "user-select" "none") ]
            ++ (case action of
                    Just msg ->
                        [ Events.onClick msg, pointer, mouseOver [ Background.color Color.uiLightGray ] ]

                    Nothing ->
                        [ Font.color Color.dimmedText ]
               )
            ++ extraBorder
        )
        [ el [ centerY, Font.size 20, alignTop, centerY ] (text symbol)
        , el [ centerY, UI.uiFont, Font.hairline, Font.size 20, centerY ] (text desc)
        ]


toolbar : App.Model -> Element App.Msg
toolbar model =
    case model.state of
        App.InboxOpen ->
            row []
                [ toolbarButton False "⟳" "Refresh" (Just App.DoNothing) ]

        App.ThreadOpen { location } ->
            let
                thread =
                    getThread location.inboxIndex model

                archive canArchive =
                    if canArchive then
                        toolbarButton False "↓" "Archive" (Just App.ArchiveThread)

                    else
                        toolbarButton False "↓" "Archive" Nothing
            in
            row []
                [ toolbarButton False "←" "Return to Inbox" (Just App.NavBack)
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
                [ el [ centerX, UI.contentFont, Font.color Color.dimmedText ] <| text "You're all done!"
                , el [ centerX, UI.contentFont, Font.color Color.dimmedText, Font.size 10 ] <| text "Nothing in Inbox"
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
            , starred = False
            , size = -1024
            }


threadPreview : App.Model -> Int -> App.ActiveThread -> Maybe (Element App.Msg)
threadPreview model inboxIndex { scriptId, contents, state, starred, size } =
    if state == App.Archived then
        Nothing

    else
        let
            ( weight, bgColor, important ) =
                case state of
                    App.Unread { responseOptions } ->
                        ( Font.bold, Color.white, Util.choose (List.length responseOptions > 0) Assets.importantYes Assets.importantNo )

                    App.Unresponded { responseOptions } ->
                        ( Font.regular, Color.uiLightGray, Util.choose (List.length responseOptions > 0) Assets.importantYes Assets.importantNo )

                    App.Responded _ ->
                        ( Font.regular, Color.uiLightGray, Assets.importantNo )

                    App.Archived ->
                        ( Font.regular, Color.uiLightGray, Assets.importantNo )

            location =
                { scriptId = scriptId, inboxIndex = inboxIndex }
        in
        row
            [ width fill, height UI.threadHeight, Background.color bgColor ]
            [ el [ width UI.leftBuffer1, centerY, Events.onClick (App.ToggleStar scriptId) ] (el [ centerX ] (Element.html (Util.choose starred Assets.starYes Assets.starNo)))
            , el [ width UI.leftBuffer2, centerY ] (el [ centerX ] (Element.html important))
            , row [ width fill, height fill, pointer, Events.onClick (App.NavPushUrl ("/k/inbox/" ++ location.scriptId)), UI.contentFont ]
                [ el
                    [ weight, width (px 230), height fill, clipX ]
                    (el [ centerY ] (text <| getThreadParticipants model contents))
                , el [ width (px 20) ] none
                , el
                    [ weight, width fill, height fill ]
                    (el [ centerY ] (getScript scriptId model |> .subject |> text))
                , el
                    [ weight, width (px 150), height fill ]
                    (el [ centerY, Element.alignRight, UI.uiFont ] (text (prettySize size)))
                , el [ width UI.rightBuffer, height UI.threadHeight, Element.alignRight ] Element.none
                ]
            ]
            |> Just


prettySize : Int -> String
prettySize x =
    if x < 600 then
        String.fromInt x ++ " bytes"

    else if x < 1024 * 100 then
        String.fromInt (x // 1024) ++ " KB"

    else if x < 1024 * 1024 then
        "0." ++ String.fromInt (x * 10 // 1024 // 1024) ++ " MB"

    else
        String.fromInt (x // 1024 // 1024) ++ " MB"


getThreadParticipants : App.Model -> List Script.Email -> String
getThreadParticipants model emails =
    List.map .from emails
        |> List.uniqueBy .email
        |> List.map
            (\sender ->
                if sender.email == model.me.email then
                    { email = sender.email, short = "me", full = "me" }

                else
                    sender
            )
        |> (\senders ->
                case senders of
                    [ { full } ] ->
                        full

                    _ ->
                        List.map .short senders |> List.intersperse ", " |> String.concat
           )



{- }

   List.map .from emails
       -- uniq
       |> List.foldl
           (\sender ( set, uniq ) ->
               if Set.member sender.email set then
                   ( set, uniq )

               else
                   ( Set.insert sender.email set, sender :: uniq )
           )
           ( Set.empty, [] )
       |> (\( _, uniq ) -> List.reverse uniq)
-}


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
                , el [ Font.size 24, UI.contentFont ] (text script.subject)
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
        ( fontColor, backgroundColor, borderColor ) =
            if selected then
                ( Color.white, Color.suggestionColor, Color.suggestionColor )

            else
                ( Color.suggestionColor, Color.white, Color.uiGray )
    in
    el
        [ Font.color fontColor
        , Background.color backgroundColor
        , Events.onClick (App.ToggleSuggestion suggestionIndex)
        , Border.color borderColor
        , Border.solid
        , Border.width 1
        , Border.rounded 5
        , paddingXY 20 10
        , width shrink
        , UI.contentFont
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
                    [ viewResponse "TO:" emailResponse.email.to
                    , column [ spacing 10 ] (List.map (\par -> paragraph [ UI.contentFont ] [ text par ]) emailResponse.email.contents)
                    , UI.separator
                    , toolbarButton True "→" "Send" (Just App.SelectSuggestion)
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
            row [ width fill, spacing 15, UI.uiFont ] [ text kind, wrappedRow [ width fill, spacing 15 ] (List.map toPill records) ]


toPill : Script.AddressbookEntry -> Element msg
toPill record =
    el [ paddingXY 10 0, height (px 22), Border.width 1, Border.rounded 10, Font.size 15, Border.color (rgb255 255 140 0), UI.contentFont ] (el [ centerY ] (text record.full))


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
                [ el [ Font.bold, UI.contentFont ] (text email.from.full)
                , el [ Font.color Color.dimmedText, UI.contentFont ] (text ("  <" ++ email.from.email ++ ">"))
                ]
                :: row [ Font.size 15, Font.color Color.dimmedText, spacing 4 ] [ el [ UI.uiFont ] (text "TO:"), el [ UI.contentFont ] (text to) ]
                :: List.map (text >> List.singleton >> paragraph [ UI.contentFont ]) email.contents
            )
        , el [ width UI.rightBuffer ] none
        ]
