module Main exposing (..)

import Browser
import Dict
import Element exposing (Element, el, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html, button, div)
import Html.Events exposing (onClick)


type alias Model =
    { contacts : Dict.Dict String String
    , threads : Dict.Dict String Thread
    , currentThreads : List String
    , currentEmail : Maybe String
    }


type alias Thread =
    List Email


type alias Email =
    { from : String
    , time : Int
    , to : List String
    , cc : List String
    , bcc : List String
    , body : List String
    }


type Msg
    = No


init : Model
init =
    { contacts =
        Dict.fromList
            [ ( "rob@rob.net", "Rob Simmons" )
            , ( "chris@phone", "Chris Martens" )
            ]
    , threads =
        Dict.fromList
            [ ( "argue", [ { from = "rob@rob.net", time = 12, to = [ "chris@phone" ], cc = [], bcc = [], body = [ "Hi", "There", "Chris" ] } ] )
            ]
    , currentThreads = [ "argue" ]
    , currentEmail = Nothing
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


update msg model =
    ( model, Cmd.none )



-- Subscriptions


subscriptions model =
    Sub.none



-- View


view : Model -> Html msg
view model =
    Element.layout [ Element.height Element.fill, Element.width Element.fill ] myRowOfStuff



--    Element.layout []
--            myRowOfStuff


myRowOfStuff =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 0
        ]
        [ left
        , right
        ]


left : Element msg
left =
    el
        [ Element.width (Element.px 250)
        , Element.height Element.fill
        , Background.color (Element.rgb255 200 200 200)
        ]
        (Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.spacing 20
            ]
            [ el [] Element.none
            , el [ Element.centerX ] (text "Gabble")
            , text "BAR"
            , text "BAZ"
            ]
        )


right : Element msg
right =
    Element.column
        [ Element.height Element.fill
        , Element.width Element.fill
        ]
        [ el
            [ Element.width Element.fill
            , Element.height (Element.px 40)
            , Background.color (Element.rgb255 44 255 255)
            ]
            (text "Header 1")
        , el
            [ Element.width Element.fill
            , Element.height (Element.px 40)
            , Background.color (Element.rgb255 44 100 255)
            ]
            (text "Header 2")
        , el 
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.scrollbarY
            ]
            (Element.column [ Element.spacing 20 ]
                [ text "Email 22"
                , text "Email 21"
                , text "Email 20"
                , text "Email 19"
                , text "Email 18"
                , text "Email 17"
                , text "Email 16"
                , text "Email 15"
                , text "Email 14"
                , text "Email 13"
                , text "Email 12"
                , text "Email 11"
                , text "Email 10"
                , text "Email 9"
                , text "Email 8"
                , text "Email 7"
                , text "Email 6"
                , text "Email 5"
                , text "Email 4"
                , text "Email 3"
                , text "Email 2"
                , text "Email 1"
                ]
            )
        , el
            [ Element.width Element.fill
            , Element.height (Element.px 40)
            , Background.color (Element.rgb255 44 255 255)
            ]
            (text "Footer")
        ]


myElement : Element msg
myElement =
    el
        [ Background.color (Element.rgb255 240 0 245)
        , Font.color (Element.rgb255 255 255 255)
        , Border.rounded 3
        , Element.padding 30
        ]
        (text "horrid!")
