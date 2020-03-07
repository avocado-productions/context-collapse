module Main2 exposing (main)

import Browser
import Cmd.Extra as C
import Dict exposing (Dict)
import Element exposing (Element, centerX, centerY, column, el, fill, height, paragraph, px, rgb255, row, spacing, text, width)
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
    = ReturnToInbox
    | OpenThread ActiveThread
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
        |> C.with Cmd.none



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReturnToInbox ->
            { model | currentThread = Nothing } |> C.with Cmd.none

        OpenThread thread ->
            { model | currentThread = Just thread } |> C.with Cmd.none

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
            { model | script = model.script }
                |> C.with Cmd.none

keyEnabled key enabled used =
    case (key, used) of
        (Nothing, Nothing) -> Just (Set.empty) -- Root emails are enabled if no root has been published
        (Nothing, Just _) -> Nothing -- Root emails are disabled once a root is published
        (Just _, Nothing) -> Nothing -- No named emails are enabled until a root is published
        (Just k, Just u) -> 
            if (Set.member k enabled) && not (Set.member k u)
            then Just (Set.insert k u)
            else Nothing

checkScriptForEnabledMessage : Set String -> Maybe (Set String) -> List Script.Step -> Maybe (Script.ScriptComponent, Set String)
checkScriptForEnabledMessage enabled used script =
    case script of
        [] -> Nothing
        step :: steps ->
            case keyEnabled step.key enabled used of
                Nothing -> checkScriptForEnabledMessage enabled used steps
                Just newUsed -> 
                    --- TODO ADD CONDITIONS HERE
                    if List.length step.guards == 0
                    then Just (step.contents, newUsed)
                    else checkScriptForEnabledMessage enabled used steps

advanceScript : 

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
            (el [ centerY ] (text "AAA"))
        , el
            [ weight, width fill, height fill, Element.pointer, Events.onClick (OpenThread thread) ]
            (el [ centerY ] (text "BBB"))
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
            [ el [ height (px 10) ] Element.none
            , el [ Font.size 30 ] (text thread.subject)
            , el [ height (px 10) ] Element.none
            , paragraph [] <| List.singleton <| text "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
            , paragraph [] <| List.singleton <| text "Why do we use it?"
            , paragraph [] <| List.singleton <| text "It is a long established fact that a reader will be distracted by the readable content of a page when looking at its layout. The point of using Lorem Ipsum is that it has a more-or-less normal distribution of letters, as opposed to using 'Content here, content here', making it look like readable English. Many desktop publishing packages and web page editors now use Lorem Ipsum as their default model text, and a search for 'lorem ipsum' will uncover many web sites still in their infancy. Various versions have evolved over the years, sometimes by accident, sometimes on purpose (injected humour and the like)."
            , paragraph [] <| List.singleton <| text "Where does it come from?"
            , paragraph [] <| List.singleton <| text "Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words, consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source. Lorem Ipsum comes from sections 1.10.32 and 1.10.33 of \"de Finibus Bonorum et Malorum\" (The Extremes of Good and Evil) by Cicero, written in 45 BC. This book is a treatise on the theory of ethics, very popular during the Renaissance. The first line of Lorem Ipsum, \"Lorem ipsum dolor sit amet..\", comes from a line in section 1.10.32."
            ]
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

                Just _ ->
                    el [ Events.onClick ReturnToInbox, centerX, centerY, Element.pointer ] (text "<-")
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
