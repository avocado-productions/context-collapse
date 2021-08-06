module CryptoScript exposing (hash, hashScript)

import Crypto.Hash as Hash
import Dict
import Script as Script exposing (Script)


hash : String -> String
hash str =
    Hash.sha224 str
        |> String.slice 0 15


hashThread : Script.ThreadScript -> Script.ThreadScript
hashThread thread =
    { thread
        | id = hash thread.id
        , scenes = Dict.map hashScene thread.scenes
    }

{-| Replace author-defined threadnames with hashes that one would be more likely to see in an email client
-}

hashScript : Script -> Script
hashScript script =
    { script | threads = List.map hashThread script.threads }


hashScene : a -> Script.ThreadScene -> Script.ThreadScene
hashScene _ scene =
    { scene | actions = List.map hashAction scene.actions }


hashAction : Script.Action -> Script.Action
hashAction action =
    case action of
        Script.Respond emailResponse ->
            Script.Respond { emailResponse | spawn = List.map hash emailResponse.spawn }

        Script.Immediate str ->
            Script.Immediate str

        Script.Archive ->
            Script.Archive
