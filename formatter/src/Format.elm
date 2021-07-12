port module Format exposing (main)

import Cmd.Extra as Cmd
import CodeGen
import Json.Decode
import Json.Encode
import Platform exposing (Program)
import Result.Extra as Result


port response : Json.Encode.Value -> Cmd msg


main : Program Json.Decode.Value () ()
main =
    Platform.worker
        { init = \input -> ( (), response (theFunctionThatDoesThings input) )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


theFunctionThatDoesThings : Json.Decode.Value -> Json.Encode.Value
theFunctionThatDoesThings input =
    Json.Decode.decodeValue Json.Decode.string input
        |> Result.map
            (\str ->
                Json.Encode.object
                    [ ( "elmCode", Json.Encode.string <| CodeGen.compileToElmCode str )
                    ]
            )
        |> Result.mapError
            (\err ->
                Json.Encode.object
                    [ ( "error", Json.Encode.bool True )
                    , ( "msg", Json.Encode.string <| Json.Decode.errorToString err )
                    ]
            )
        |> Result.merge

