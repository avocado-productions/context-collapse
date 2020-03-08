module View exposing (view)

import AppTypes as App
import Assets
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html exposing (Html)
import List.Extra
import ScriptTypes as Script


view : App.Model -> Html App.Msg
view model =
    Element.layout
        [ height fill
        , width fill
        ]
        (browserUI model)


suggestionColor : Color
suggestionColor =
    rgb255 50 130 255


buttonSpacing : Int
buttonSpacing =
    20


suggestionButton : Bool -> Int -> Int -> String -> Element App.Msg
suggestionButton selected threadIndex suggestionIndex shortMessage =
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
        , Events.onClick (App.ToggleSuggestion threadIndex suggestionIndex)
        , Border.color suggestionColor
        , Border.solid
        , Border.width 1
        , Border.rounded 5
        , paddingXY 20 10
        , width shrink
        ]
        (text shortMessage)


browserUI : App.Model -> Element App.Msg
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


viewInbox : List App.ActiveThread -> Element App.Msg
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


leftBuffer : Element.Length
leftBuffer =
    px 70


leftBuffer1 : Element.Length
leftBuffer1 =
    px 35


leftBuffer2 : Element.Length
leftBuffer2 =
    px 35


rightBuffer : Element.Length
rightBuffer =
    px 35


viewThreadPreview : App.ActiveThread -> Element App.Msg
viewThreadPreview thread =
    let
        ( weight, bgColor, important ) =
            case thread.state of
                App.Unread _ _ ->
                    ( Font.bold, rgb255 255 255 255, Assets.importantYes )

                App.Unresponded _ _ ->
                    ( Font.regular, rgb255 240 240 240, Assets.importantYes )

                App.Responded ->
                    ( Font.regular, rgb255 240 240 240, Assets.importantNo )
    in
    row
        [ width fill, height threadHeight, Background.color bgColor ]
        [ el [ width leftBuffer1, centerY ] (el [ centerX ] (Element.html Assets.starNo))
        , el [ width leftBuffer2, centerY ] (el [ centerX ] (Element.html important))
        , el
            [ weight, width (px 250), height fill, Element.pointer, Events.onClick (App.OpenThread thread.index) ]
            (el [ centerY ] (text "(todo: participants)"))
        , el
            [ weight, width fill, height fill, Element.pointer, Events.onClick (App.OpenThread thread.index) ]
            (el [ centerY ] (text thread.subject))
        , el
            [ weight, width (px 150), height fill, Element.alignRight, Element.pointer, Events.onClick (App.OpenThread thread.index) ]
            (el [ centerY, Element.alignRight ] (text "1:15 PM"))
        , el [ width rightBuffer, height threadHeight, Element.alignRight ] Element.none
        ]


lookupEmail : App.Model -> String -> Script.AddressbookEntry
lookupEmail model key =
    Dict.get key model.addressbook
        |> Maybe.withDefault
            { key = key
            , email = "ERROR@INTERNAL.NO." ++ key
            , short = "ERROR " ++ key
            , full = "ERROR, NO KEY " ++ key
            }


dimmedText : Color
dimmedText =
    rgb255 120 120 120


uiGray : Color
uiGray =
    rgb255 200 200 200


viewEmail : App.Model -> Script.Email -> Element App.Msg
viewEmail model email =
    let
        from =
            lookupEmail model email.from

        to =
            List.map (lookupEmail model) email.to
                |> List.map (\x -> x.full)
                |> List.intersperse ", "
                |> String.concat
    in
    row [ width fill ]
        [ el [ width leftBuffer, centerX, alignTop ] (html Assets.idCircle)
        , column [ width fill, spacing 10 ]
            (paragraph [ Font.size 15 ]
                [ el [ Font.bold ] (text from.full)
                , el [ Font.color dimmedText ] (text ("  <" ++ from.email ++ ">"))
                ]
                :: paragraph [ Font.size 15, Font.color dimmedText ] [ text ("to " ++ to) ]
                :: List.map (text >> List.singleton >> paragraph []) email.contents
            )
        ]


separator : Element msg
separator =
    el [ height (px 1), Background.color uiGray, width fill ] none


responseSeparator : Length
responseSeparator =
    px 20


toPill : String -> Element msg
toPill name =
    el [ paddingXY 10 0, height (px 22), Border.width 1, Border.rounded 10, Font.size 15, Border.color (rgb255 255 140 0) ] (el [ centerY ] (text name))


viewResponse : App.Model -> String -> List String -> Element msg
viewResponse model kind entries =
    let
        names =
            List.map
                (\id ->
                    Dict.get id model.addressbook
                        |> Maybe.map (\entry -> entry.full)
                        |> Maybe.withDefault ("ERROR UNKNOWN " ++ id)
                )
                entries
    in
    case entries of
        [] ->
            none

        _ ->
            row [ width fill, spacing 15 ] [ text kind, wrappedRow [ width fill, spacing 15 ] (List.map toPill names) ]


viewEmailResponse : App.Model -> Int -> Int -> Script.EmailResponse -> Element App.Msg
viewEmailResponse model threadIndex suggestionIndex emailResponse =
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
                    [ viewResponse model "To" emailResponse.email.to
                    , column [ spacing 10 ] (List.map (\par -> paragraph [] [ text par ]) emailResponse.email.contents)
                    , separator
                    , el
                        [ Events.onClick (App.MakeDecision threadIndex emailResponse)
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


viewSuggestions : App.Model -> Int -> List Script.EmailResponse -> Maybe Int -> Element App.Msg
viewSuggestions model threadIndex responseOptions selectedIndex =
    column [ width fill ]
        [ row [ width fill ]
            [ el [ width leftBuffer ] none
            , wrappedRow [ spacing buttonSpacing, width fill ]
                (List.indexedMap
                    (\suggestionIndex responseOption ->
                        suggestionButton
                            (selectedIndex == Just suggestionIndex)
                            threadIndex
                            suggestionIndex
                            responseOption.shortText
                    )
                    responseOptions
                )
            ]
        , (selectedIndex
            |> Maybe.andThen
                (\responseIndex ->
                    List.Extra.getAt responseIndex responseOptions
                        |> Maybe.map
                            (\emailResponse ->
                                viewEmailResponse model threadIndex responseIndex emailResponse
                            )
                )
          )
            |> Maybe.withDefault none
        ]


viewThread : App.Model -> Int -> App.ActiveThread -> Element App.Msg
viewThread model threadIndex thread =
    column [ width fill, height fill, spacing 20 ]
        (el [ height (px 10) ] Element.none
            :: row [ width fill ]
                [ el [ width leftBuffer ] none
                , el [ Font.size 24 ] (text thread.subject)
                ]
            :: (List.map (viewEmail model) thread.contents |> List.intersperse separator)
            ++ [ case thread.state of
                    App.Responded ->
                        Element.none

                    App.Unread responseOptions selectedIndex ->
                        viewSuggestions model threadIndex responseOptions selectedIndex

                    App.Unresponded responseOptions selectedIndex ->
                        viewSuggestions model threadIndex responseOptions selectedIndex
               ]
        )


mainPanel : App.Model -> Element App.Msg
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
                    el [ Events.onClick (App.ReturnToInbox activeThread), centerX, centerY, Element.pointer ] (text "<-")
            )
        , el [ width fill, height (px 1), Background.color (rgb255 200 200 200) ] Element.none
        , el
            [ width fill
            , height fill
            , Element.scrollbarY
            ]
            (case
                model.currentThread
                    |> Maybe.andThen
                        (\i ->
                            List.Extra.getAt i model.inbox
                                |> Maybe.map (\thread -> ( i, thread ))
                        )
             of
                Nothing ->
                    viewInbox model.inbox

                Just ( threadIndex, thread ) ->
                    viewThread model threadIndex thread
            )
        ]
