module View exposing (view)

import App as App exposing (Msg(..))
import Assets
import Browser exposing (Document)
import Color
import Dict
import Dict.Extra as Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Region as Region
import Html.Attributes
import List.Extra as List
import Markup exposing (Markup)
import Message exposing (Message)
import Props exposing (Props)
import Props2 as Props
import Script
import UI
import Util


view : App.Model -> Document App.Msg
view model =
    let
        openThread =
            Dict.find (\_ thread -> Props.getFlag "open" thread.props) model.threads
                |> Maybe.map Tuple.second
    in
    { title =
        case model.state of
            App.InboxOpen ->
                let
                    unread =
                        getOrderedInbox model
                            |> List.filter (\{ props } -> Props.getFlag "unread" props)
                            |> List.length
                in
                if unread == 0 then
                    "Inbox - " ++ model.script.me.email ++ " - AvoComm"

                else
                    "Inbox (" ++ String.fromInt unread ++ ") - " ++ model.script.me.email ++ " - AvoComm"

            App.ThreadOpen { threadId } ->
                let
                    script =
                        getScript threadId model
                in
                script.subject ++ " - " ++ model.script.me.email ++ " - AvoComm"
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
                        toolbar model openThread
                    , el [ width fill, height (px 10) ] <|
                        none
                    , el [ width fill, height (px 1), Background.color Color.uiGray ] <|
                        none
                    , el [ width fill, height fill, scrollbarY ] <|
                        case openThread of
                            Nothing ->
                                viewAllThreadsInInbox model

                            Just thread ->
                                viewSingleThread model thread
                    ]
                ]
        ]
    }


getOrderedInbox : App.Model -> List App.ActiveThread
getOrderedInbox model =
    List.filterMap (\{ threadId } -> Dict.get threadId model.threads) model.inbox


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


toolbar : App.Model -> Maybe App.ActiveThread -> Element App.Msg
toolbar model openThread =
    case openThread of
        Nothing ->
            row []
                [ toolbarButton False "⟳" "Refresh" (Just App.DoNothing) ]

        Just thread ->
            let
                archive =
                    if Props.getFlag "archivable" thread.props then
                        toolbarButton False "↓" "Archive" (Just (App.Archive { threadId = thread.threadId }))

                    else
                        toolbarButton False "↓" "Archive" Nothing
            in
            row []
                [ toolbarButton False "←" "Return to Inbox" (Just App.NavBack)
                , archive
                ]


viewAllThreadsInInbox : App.Model -> Element App.Msg
viewAllThreadsInInbox model =
    if Dict.values model.threads |> List.all (\{ props } -> Props.getFlag "archived" props) then
        el [ Background.color Color.uiGray, width fill, height fill ] <|
            column [ centerY, spacing 20, width fill ]
                [ el [ centerX, UI.contentFont, Font.color Color.dimmedText ] <| text "You're all done!"
                , el [ centerX, UI.contentFont, Font.color Color.dimmedText, Font.size 10 ] <| text "Nothing in Inbox"
                ]

    else
        column [ Background.color Color.uiGray, spacing 1, width fill, height fill ] <|
            (model.inbox
                |> List.filterMap (\{ threadId } -> Dict.get threadId model.threads)
                |> List.map (threadPreview model)
                |> List.filterMap identity
            )


getScript : String -> App.Model -> Script.ThreadScript
getScript needle model =
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
            , props = Props.empty
            }


threadPreview : App.Model -> App.ActiveThread -> Maybe (Element App.Msg)
threadPreview model { threadId, contents, state, props } =
    if Props.getFlag "archived" props then
        Nothing

    else
        let
            important =
                Props.getMaybeBool "important" props
                    |> Maybe.withDefault False
                    -- TODO
                    |> Util.choose Assets.importantYes Assets.importantNo

            ( weight, bgColor ) =
                if Props.getFlag "unread" props then
                    ( Font.bold, Color.white )

                else
                    ( Font.regular, Color.uiLightGray )
        in
        row
            [ width fill, height UI.threadHeight, Background.color bgColor ]
            [ el [ width UI.leftBuffer1, centerY, Events.onClick (App.SetFlag { threadId = threadId, key = "starred", value = not <| Props.getFlag "starred" props }) ] (el [ centerX ] (Element.html (Props.getFlag "starred" props |> Util.choose Assets.starYes Assets.starNo)))
            , el [ width UI.leftBuffer2, centerY ] (el [ centerX ] (Element.html important))
            , row [ width fill, height fill, pointer, Events.onClick (App.NavPushUrl ("/k/inbox/" ++ threadId)), UI.contentFont ]
                [ el
                    [ weight, width (px 230), height fill, clipX ]
                    (el [ centerY ] (text <| getThreadParticipants model contents))
                , el [ width (px 20) ] none
                , el
                    [ weight, width fill, height fill ]
                    (el [ centerY ] (getScript threadId model |> .subject |> text))
                , el
                    [ weight, width (px 150), height fill ]
                    (el [ centerY, Element.alignRight, UI.uiFont ] (text (prettySize (Props.getInt "size" props))))
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


getAddressBookEntry : App.Model -> String -> Script.AddressbookEntry
getAddressBookEntry { script } id =
    Dict.get id script.addressBook
        |> Maybe.withDefault
            { email = "XXX_ERROR_MISSING_" ++ id
            , full = "XXX_ERROR_MISSING_" ++ id
            , short = "XXX_ERROR_MISSING_" ++ id
            }


getThreadParticipants : App.Model -> List Message -> String
getThreadParticipants model emails =
    List.map (\{ props } -> getFrom model props :: getTo model props) emails
        |> List.concat
        |> List.uniqueBy .full
        |> List.map
            (\addressbookEntry ->
                if addressbookEntry.email == model.script.me.email then
                    { email = addressbookEntry.email, short = "me", full = "me" }

                else
                    addressbookEntry
            )
        |> (\participants ->
                case participants of
                    [ { full } ] ->
                        full

                    _ ->
                        List.map .short participants |> List.intersperse ", " |> String.concat
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


viewSingleThread : App.Model -> App.ActiveThread -> Element App.Msg
viewSingleThread model thread =
    let
        script =
            getScript thread.threadId model
    in
    column [ width fill, height fill, spacing 20 ]
        (el [ height (px 10) ] Element.none
            :: row [ width fill ]
                [ el [ width UI.leftBuffer ] none
                , el [ Font.size 24, UI.contentFont ] (text script.subject)
                ]
            :: (List.map (viewEmail model) thread.contents |> List.intersperse UI.separator)
            ++ [ case thread.state of
                    App.Ready { responseOptions } ->
                        suggestionPicker model thread.threadId thread.props responseOptions

                    App.Waiting ->
                        none
               ]
        )


suggestionButton : String -> Bool -> Int -> Markup -> Element App.Msg
suggestionButton threadId selected suggestionIndex shortMessage =
    let
        ( fontColor, backgroundColor, borderColor ) =
            if selected then
                ( Color.white, Color.suggestionColor, Color.suggestionColor )

            else
                ( Color.suggestionColor, Color.white, Color.uiGray )
    in
    row
        [ Font.color fontColor
        , Background.color backgroundColor
        , Events.onClick (App.SetMaybeIntProp { threadId = threadId, key = "selection", value = selected |> Util.choose Nothing (Just suggestionIndex) })
        , Border.color borderColor
        , Border.solid
        , Border.width 1
        , Border.rounded 5
        , paddingXY 20 10
        , width shrink
        , UI.contentFont
        ]
        (viewMarkup shortMessage)


suggestionPicker : App.Model -> String -> Props -> List Script.EmailResponse -> Element App.Msg
suggestionPicker model threadId props responseOptions =
    let
        currentSelection =
            Props.getMaybeInt "selection" props
                |> Maybe.withDefault -1
    in
    column [ width fill ]
        [ -- Selections
          row [ width fill ]
            [ el [ width UI.leftBuffer ] none
            , wrappedRow [ spacing UI.buttonSpacing, width fill ]
                (List.indexedMap
                    (\suggestionIndex responseOption ->
                        suggestionButton
                            threadId
                            (currentSelection == suggestionIndex)
                            suggestionIndex
                            responseOption.shortText
                    )
                    responseOptions
                )
            ]
        , -- Contents of selected email
          List.getAt currentSelection responseOptions
            |> Maybe.map (viewEmailResponse model threadId currentSelection)
            |> Maybe.withDefault none
        ]


getFrom : App.Model -> Props -> Script.AddressbookEntry
getFrom model props =
    Props.getString "from" props
        |> getAddressBookEntry model


getTo : App.Model -> Props -> List Script.AddressbookEntry
getTo model props =
    Props.getStrings "to" props
        |> List.map (getAddressBookEntry model)


viewEmailResponse : App.Model -> String -> Int -> Script.EmailResponse -> Element App.Msg
viewEmailResponse model threadId index emailResponse =
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
                    [ viewResponse "TO:" (getTo model emailResponse.email.props)
                    , viewEmailContents emailResponse.email.contents
                    , UI.separator
                    , toolbarButton True "→" "Send" (Just <| App.Select threadId index)
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


viewEmailContents : List Message.Element -> Element App.Msg
viewEmailContents elements =
    column [ width fill, spacing 10 ] <|
        List.map
            (\element ->
                case element of
                    Message.Paragraph markup ->
                        paragraph [ UI.contentFont ] (viewMarkup markup)

                    Message.Image { url } ->
                        image [] { src = url, description = "" }

                    Message.Quote _ ->
                        paragraph [ UI.contentFont ] [ text <| "QUOTED SECTION" ]
            )
            elements


viewStyle : { a | bold : Bool, italic : Bool, strike : Bool } -> List (Attribute msg)
viewStyle { bold, italic, strike } =
    (if bold then
        [ Font.bold ]

     else
        []
    )
        ++ (if italic then
                [ Font.italic ]

            else
                []
           )
        ++ (if strike then
                [ Font.strike ]

            else
                []
           )


viewMarkup : Markup -> List (Element App.Msg)
viewMarkup =
    List.map
        (\element ->
            case element of
                Markup.Raw style str ->
                    [ el (viewStyle style) (text str) ]

                Markup.Link { contents } ->
                    -- TODO ADD STYLING AND USE URL
                    viewMarkup contents
        )
        >> List.concat



-- text >> List.singleton >> paragraph [ UI.contentFont ]


viewEmail : App.Model -> Message -> Element App.Msg
viewEmail model email =
    let
        to =
            getTo model email.props
                |> List.map .full
                |> List.intersperse ", "
                |> String.concat

        from =
            getFrom model email.props
    in
    row [ width fill ]
        [ el [ width UI.leftBuffer, centerX, alignTop ] (html Assets.idCircle)
        , column [ width fill, spacing 10 ]
            [ paragraph [ Font.size 15 ]
                [ el [ Font.bold, UI.contentFont ] (text from.full)
                , el [ Font.color Color.dimmedText, UI.contentFont ] (text ("  <" ++ from.email ++ ">"))
                ]
            , row [ Font.size 15, Font.color Color.dimmedText, spacing 4 ] [ el [ UI.uiFont ] (text "TO:"), el [ UI.contentFont ] (text to) ]
            , viewEmailContents email.contents
            ]
        , el [ width UI.rightBuffer ] none
        ]
