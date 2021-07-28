module App exposing (main)

import AppTypes as App
import AvoComm
import Browser
import Cmd.Extra as Cmd
import Html
import Parse.Parse as Parse
import View


run ( model, cmd ) =
    ( Run model, cmd )


type Model
    = Run App.Model
    | Show { script : String, err : String }


main : Program String Model App.Msg
main =
    Browser.application
        { init =
            \scriptStr url key ->
                case Parse.parse scriptStr of
                    Err msg ->
                        Show { script = scriptStr, err = msg } |> Cmd.pure

                    Ok script ->
                        AvoComm.init script url key |> run
        , view =
            \model ->
                case model of
                    Run appmodel ->
                        View.view appmodel

                    Show { script, err } ->
                        { title = "Error parsing script.camp"
                        , body =
                            [ Html.pre [] [ Html.text err ]
                            , Html.pre [] [ Html.text script ]
                            ]
                        }
        , update =
            \msg model ->
                case model of
                    Run appmodel ->
                        AvoComm.update msg appmodel |> run

                    Show _ ->
                        model |> Cmd.pure
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = App.OnUrlRequest
        , onUrlChange = App.OnUrlChange
        }
