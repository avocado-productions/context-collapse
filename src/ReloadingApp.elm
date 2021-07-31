module ReloadingApp exposing (..)

import App
import AvoComm
import Browser
import Browser.Navigation
import Html
import Html.Attributes
import Http
import Loading
import Parse.Parse as Parse
import Process
import Task
import Url
import View


type alias Model =
    { state : State
    , url : Url.Url
    , key : Browser.Navigation.Key
    }


type State
    = Loading
    | LoadError { error : Http.Error }
    | ParseError { msg : String, source : String }
    | AvoCommModel { source : String, avoCommModel : App.Model }


type Msg
    = GotMsg String
    | GotErr Http.Error
    | AvoCommMsg App.Msg
    | Poll ()
    | Noop


pollLater : Cmd Msg
pollLater =
    Task.perform Poll (Process.sleep 10000)


pollNow : Cmd Msg
pollNow =
    Http.get
        { url = "script.camp"
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


main : Program () Model Msg
main =
    Browser.application
        { init = \() url key -> ( { url = url, key = key, state = Loading }, pollLater )
        , view =
            \model ->
                case model.state of
                    Loading ->
                        { title = "Loading..."
                        , body =
                            [ Html.div
                                [ Html.Attributes.style "padding" "200px" ]
                                [ Loading.render Loading.Spinner Loading.defaultConfig Loading.On ]
                            ]
                        }

                    LoadError error ->
                        { title = "Connection error", body = [ Html.text (Debug.toString model) ] }

                    ParseError { msg, source } ->
                        { title = "Could not parse script", body = [ Html.text (Debug.toString model) ] }

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
                                    ( model, pollLater )

                                else
                                    restart model newSource

                            _ ->
                                restart model newSource

                    GotErr error ->
                        case model.state of
                            Loading ->
                                ( { model | state = LoadError { error = error } }, pollLater )

                            LoadError _ ->
                                ( { model | state = LoadError { error = error } }, pollLater )

                            ParseError _ ->
                                ( model, pollLater )

                            AvoCommModel _ ->
                                ( model, pollLater )

                    Poll () ->
                        ( model, pollNow )

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
            , pollLater
            )

        Ok script ->
            AvoComm.init script model.url model.key
                |> (\( avoCommModel, avoCommCmd ) ->
                        ( { model | state = AvoCommModel { source = source, avoCommModel = avoCommModel } }
                        , Cmd.batch [ Cmd.map AvoCommMsg avoCommCmd, pollLater ]
                        )
                   )
