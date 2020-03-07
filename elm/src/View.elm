module View exposing (view)

import AppTypes as App
import Assets
import Element exposing (..)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Html exposing (Html)


view : App.Model -> Html App.Msg
view model =
    Element.layout
        [ height fill
        , width fill
        ]
        (browserUI model)


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


viewThreadPreview : App.ActiveThread -> Element App.Msg
viewThreadPreview thread =
    let
        ( weight, bgColor, important ) =
            case thread.state of
                App.Unread _ ->
                    ( Font.bold, rgb255 255 255 255, Assets.importantYes )

                App.Unresponded _ ->
                    ( Font.regular, rgb255 240 240 240, Assets.importantYes )

                App.Responded ->
                    ( Font.regular, rgb255 240 240 240, Assets.importantNo )
    in
    row
        [ width fill, height threadHeight, Background.color bgColor ]
        [ el [ width threadHeight, centerY ] (el [ centerX ] (Element.html Assets.starNo))
        , el [ width threadHeight, centerY ] (el [ centerX ] (Element.html important))
        , el
            [ weight, width (px 250), height fill, Element.pointer, Events.onClick (App.OpenThread thread) ]
            (el [ centerY ] (text "(todo: participants)"))
        , el
            [ weight, width fill, height fill, Element.pointer, Events.onClick (App.OpenThread thread) ]
            (el [ centerY ] (text thread.subject))
        , el
            [ weight, width (px 150), height fill, Element.alignRight, Element.pointer, Events.onClick (App.OpenThread thread) ]
            (el [ centerY, Element.alignRight ] (text "1:15 PM"))
        , el [ width threadHeight, height threadHeight, Element.alignRight ] Element.none
        ]


viewThread : App.ActiveThread -> Element App.Msg
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
                        App.Responded ->
                            Element.none

                        App.Unread options ->
                            row [ width fill, spacing 10 ]
                                (List.map
                                    (\responseOption ->
                                        el [ padding 10, Background.color (rgb255 200 200 200), Events.onClick (App.MakeDecision thread.index responseOption) ]
                                            (text responseOption.shortText)
                                    )
                                    options
                                )

                        App.Unresponded options ->
                            row [ width fill, spacing 10 ]
                                (List.map
                                    (\responseOption ->
                                        el [ padding 10, Background.color (rgb255 200 200 200), Events.onClick (App.MakeDecision thread.index responseOption) ]
                                            (text responseOption.shortText)
                                    )
                                    options
                                )
                   ]
            )
        , el [ width threadHeight, height threadHeight ] Element.none
        ]


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
                    el [ Events.onClick (App.ReturnToInbox activeThread.index), centerX, centerY, Element.pointer ] (text "<-")
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
