module Main exposing (main)

import Browser
import Dict
import Element exposing (Element, centerX, centerY, column, el, fill, height, padding, paddingXY, paragraph, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes


type alias Model =
    { contacts : Dict.Dict String String
    , threads : Dict.Dict String Thread
    , currentInbox : List Thread
    , currentThread : Maybe Thread
    }


type ThreadState
    = Unread Int
    | Unresponded
    | Responded


type alias Thread =
    { emails : List Email
    , people : String
    , state : ThreadState
    , subject : String
    }


type alias Email =
    { from : String
    , time : Int
    , to : List String
    , cc : List String
    , bcc : List String
    , body : List String
    }


type Msg
    = OpenThread Thread
    | ReturnToInbox


thread1 : Thread
thread1 =
    { state = Unread 0, people = "Yer Mum", emails = [], subject = "Thread 1" }


thread5 : Thread
thread5 =
    { state = Unresponded, people = "Yer Dad", emails = [], subject = "Thread 5" }


thread6 : Thread
thread6 =
    { state = Unread 0, people = "Yer Dad", emails = [], subject = "Thread 6" }


thread7 : Thread
thread7 =
    { state = Responded, people = "Yer Mum", emails = [], subject = "Thread 7" }


thread8 : Thread
thread8 =
    { state = Responded, people = "Yer Dad", emails = [], subject = "Thread 8" }


thread9 : Thread
thread9 =
    { state = Unresponded, people = "Yer Mum", emails = [], subject = "Thread 9" }


threadA : Thread
threadA =
    { state = Responded, people = "Yer Dad", emails = [], subject = "Thread 10" }


threadB : Thread
threadB =
    { state = Responded, people = "Yer Dad", emails = [], subject = "Thread 11" }


threadC : Thread
threadC =
    { state = Unresponded, people = "Yer Mum", emails = [], subject = "Thread 12" }


thread2 : Thread
thread2 =
    { state = Unread 0
    , emails = []
    , people = "Yer Mum"
    , subject = "Won't you take me to"
    }


thread3 : Thread
thread3 =
    { state = Unread 0
    , emails = []
    , people = "Axl Rose, Slash, Izzy S…"
    , subject = "Paradise City"
    }


thread4 : Thread
thread4 =
    { state = Unread 0
    , emails = []
    , people = "Yer Mum, Axl Rose"
    , subject = "Where the grass is green"
    }


init : Model
init =
    { contacts =
        Dict.fromList
            [ ( "rob@rob.net", "Rob Simmons" )
            , ( "chris@phone", "Chris Martens" )
            ]
    , threads =
        Dict.fromList
            []
    , currentInbox = [ thread1, thread2, thread3, thread4, thread5, thread6, thread7, thread8, thread9, threadA, threadB, threadC ]
    , currentThread = Nothing
    }


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenThread thread ->
            ( { model | currentThread = Just thread }, Cmd.none )

        ReturnToInbox ->
            ( { model | currentThread = Nothing }, Cmd.none )



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


viewInbox : List Thread -> Element Msg
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


viewThreadPreview : Thread -> Element Msg
viewThreadPreview thread =
    let
        (weight, bgColor, important) =
            case thread.state of
                Unread _ -> (Font.bold, rgb255 255 255 255, importantYes)
                Unresponded -> (Font.regular, rgb255 240 240 240, importantYes)
                Responded -> (Font.regular, rgb255 240 240 240, importantNo)
    in
    row
        [ width fill, height threadHeight, Background.color bgColor ]
        [ el [ width threadHeight, centerY ] (el [ centerX ] (Element.html starNo))
        , el [ width threadHeight, centerY ] (el [ centerX ] (Element.html important))
        , el [ weight, width (px 250), height fill, Element.pointer, Events.onClick (OpenThread thread) ]
            (el [ centerY ] (text thread.people))
        , el [ weight, width fill, height fill, Element.pointer, Events.onClick (OpenThread thread) ]
            (el [ centerY ] (text thread.subject))
        , el [ weight, width (px 150), height fill, Element.alignRight, Element.pointer, Events.onClick (OpenThread thread) ]
            (el [ centerY, Element.alignRight ] (text "1:15 PM"))
        , el [ width threadHeight, height threadHeight, Element.alignRight ] Element.none
        ]


viewThread : Thread -> Element Msg
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
                    viewInbox model.currentInbox

                Just thread ->
                    viewThread thread
            )
        ]
