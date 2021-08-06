module Controller exposing (..)

import App
import AvoComm
import Browser
import Browser.Navigation
import Html
import Html.Attributes
import Http
import Json.Decode as Decode exposing (Decoder)
import Loading
import Parse.Parse as Parse
import Process
import Task
import Url
import View


type alias Model =
    { state : State
    , url : Url.Url
    , scriptLocation : String
    , pollingFrequencyInMilliseconds : Maybe Int
    , key : Browser.Navigation.Key
    }


type State
    = Loading
    | DecodeError { msg : String }
    | LoadError { error : Http.Error }
    | ParseError { msg : String, source : String }
    | AvoCommModel { source : String, avoCommModel : App.Model }


type Msg
    = GotMsg String
    | GotErr Http.Error
    | AvoCommMsg App.Msg
    | Poll ()
    | Noop


pollLater : Maybe Int -> Cmd Msg
pollLater pollingFrequencyInMilliseconds =
    case pollingFrequencyInMilliseconds of
        Nothing ->
            Cmd.none

        Just freq ->
            Task.perform Poll (Process.sleep (toFloat freq))


pollNow : String -> Cmd Msg
pollNow url =
    Http.get
        { url = url
        , expect =
            Http.expectString
                (\result ->
                    case result of
                        Ok str ->
                            GotMsg str

                        Err err ->
                            GotErr err
                )
        }


decoder : Decoder ( Maybe Int, String )
decoder =
    Decode.map2 Tuple.pair
        (Decode.field "pollingFrequencyInMilliseconds" (Decode.nullable Decode.int))
        (Decode.field "urlOfScriptFile" Decode.string)


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init =
            \flags url key ->
                case Decode.decodeValue decoder flags of
                    Ok ( pollingFrequencyInMilliseconds, urlOfScriptFile ) ->
                        ( { url = url
                          , key = key
                          , scriptLocation = urlOfScriptFile
                          , pollingFrequencyInMilliseconds = pollingFrequencyInMilliseconds
                          , state = Loading
                          }
                        , pollNow urlOfScriptFile
                        )

                    Err error ->
                        ( { url = url
                          , key = key
                          , scriptLocation = ""
                          , pollingFrequencyInMilliseconds = Nothing
                          , state = DecodeError { msg = Decode.errorToString error }
                          }
                        , Cmd.none
                        )
        , view =
            \model ->
                let
                    errorBox msg =
                        Html.div
                            [ Html.Attributes.style "width" "600px"
                            , Html.Attributes.style "background-color" "rgb(252, 196, 192)"
                            , Html.Attributes.style "border-color" "rgb(201, 117, 111)"
                            , Html.Attributes.style "border-width" "1px"
                            , Html.Attributes.style "border-style" "solid"
                            , Html.Attributes.style "border-radius" "6px"
                            , Html.Attributes.style "padding" "20px"
                            , Html.Attributes.style "margin-left" "auto"
                            , Html.Attributes.style "margin-right" "auto"
                            , Html.Attributes.style "margin-top" "50px"
                            , Html.Attributes.style "margin-bottom" "30px"
                            , Html.Attributes.style "font-family" "monospace"
                            ]
                            [ Html.text msg ]
                in
                case model.state of
                    Loading ->
                        { title = "Loading..."
                        , body =
                            [ Html.div
                                [ Html.Attributes.style "padding" "200px" ]
                                [ Loading.render Loading.Spinner Loading.defaultConfig Loading.On ]
                            ]
                        }

                    LoadError { error } ->
                        { title = "Connection error"
                        , body =
                            [ errorBox <|
                                case error of
                                    Http.Timeout ->
                                        "I tried to get the script, but the request timed out."

                                    Http.NetworkError ->
                                        "I tried to get the script, but it seems like the internet disconnected."

                                    Http.BadStatus code ->
                                        "I tried to get the script, but got an unexpected HTTP status code " ++ String.fromInt code ++ "."

                                    Http.BadBody err ->
                                        "Bad body: " ++ err

                                    Http.BadUrl err ->
                                        "Bad url : " ++ err
                            ]
                        }

                    DecodeError { msg } ->
                        { title = "Error decoding flags"
                        , body = [ errorBox msg ]
                        }

                    ParseError { msg, source } ->
                        { title = "Could not parse script"
                        , body =
                            [ Html.div
                                []
                                [ errorBox msg
                                , Html.div
                                    [ Html.Attributes.style "width" "600px"
                                    , Html.Attributes.style "border-color" "rgb(200, 210, 220)"
                                    , Html.Attributes.style "border-width" "1px"
                                    , Html.Attributes.style "border-style" "solid"
                                    , Html.Attributes.style "border-radius" "6px"
                                    , Html.Attributes.style "padding" "20px"
                                    , Html.Attributes.style "margin" "auto"
                                    ]
                                    [ Html.pre [] [ Html.text source ]
                                    ]
                                ]
                            ]
                        }

                    AvoCommModel { avoCommModel } ->
                        let
                            { title, body } =
                                View.view avoCommModel
                        in
                        { title = title, body = List.map (Html.map AvoCommMsg) body }
        , update =
            \msg model ->
                case msg of
                    GotMsg newSource ->
                        case model.state of
                            AvoCommModel { source } ->
                                if newSource == source then
                                    ( model, pollLater model.pollingFrequencyInMilliseconds )

                                else
                                    restart model newSource

                            _ ->
                                restart model newSource

                    GotErr error ->
                        case model.state of
                            Loading ->
                                ( { model | state = LoadError { error = error } }, pollLater model.pollingFrequencyInMilliseconds )

                            DecodeError _ ->
                                ( model, Cmd.none )

                            LoadError _ ->
                                ( { model | state = LoadError { error = error } }, pollLater model.pollingFrequencyInMilliseconds )

                            ParseError _ ->
                                ( model, pollLater model.pollingFrequencyInMilliseconds )

                            AvoCommModel _ ->
                                ( model, pollLater model.pollingFrequencyInMilliseconds )

                    Poll () ->
                        ( model, pollNow model.scriptLocation )

                    AvoCommMsg avoCommMsg ->
                        case model.state of
                            AvoCommModel { source, avoCommModel } ->
                                let
                                    ( newAvoCommModel, avoCommCmd ) =
                                        AvoComm.update avoCommMsg avoCommModel
                                in
                                ( { model | state = AvoCommModel { source = source, avoCommModel = newAvoCommModel } }
                                , Cmd.map AvoCommMsg avoCommCmd
                                )

                            _ ->
                                ( model, Cmd.none )

                    Noop ->
                        ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = App.OnUrlRequest >> AvoCommMsg
        , onUrlChange = App.OnUrlChange >> AvoCommMsg
        }


restart : Model -> String -> ( Model, Cmd Msg )
restart model source =
    case Parse.parse source of
        Err err ->
            ( { model | state = ParseError { source = source, msg = err } }
            , pollLater model.pollingFrequencyInMilliseconds
            )

        Ok script ->
            AvoComm.init script model.url model.key
                |> (\( avoCommModel, avoCommCmd ) ->
                        ( { model | state = AvoCommModel { source = source, avoCommModel = avoCommModel } }
                        , Cmd.batch
                            [ Cmd.map AvoCommMsg avoCommCmd
                            , pollLater model.pollingFrequencyInMilliseconds
                            ]
                        )
                   )
