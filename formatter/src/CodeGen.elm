module CodeGen exposing (compileToElmCode)

import Elm.CodeGen exposing (..)
import Elm.Pretty
import Script exposing (Script)


sTyped : String -> List TypeAnnotation -> TypeAnnotation
sTyped =
    fqTyped [ "ScriptTypes" ]


compileToElmCode : Script -> String
compileToElmCode str =
    file
        (normalModule [ "Script" ] [ funExpose "me", funExpose "script", funExpose "starting" ])
        [ importStmt [ "Contact" ] Nothing Nothing
        , importStmt [ "Dict" ] Nothing Nothing
        , importStmt [ "ScriptTypes" ] Nothing Nothing
        ]

        (addressBook <| Debug.todo)
        Nothing
        |> Elm.Pretty.pretty 120

starting : List String -> Declaration
starting threads = 
    

addressBook : List Script.Contact -> List Declaration
addressBook =
    List.map
        (\{ id, email, short, full } ->
            valDecl Nothing (Just (sTyped "AddressbookEntry" [])) ("contact_" ++ id) <|
                record [ ( "email", string email ), ( "short", string short ), ( "full", string full ) ]
        )


