module ScriptTypes exposing (..)
import Dict exposing (Dict)

type alias AddressbookEntry =
    { email : String
    , short : String
    , full : String
    }


type alias Email =
    { from : AddressbookEntry
    , to : List AddressbookEntry
    , contents : List String
    }


type alias EmailResponse =
    { shortText : String
    , email : Email
    , next : String
    }


type Action
    = Respond EmailResponse
    | Immediate String


type alias ThreadScene =
    { receivedEmail : Email
    , actions : List Action
    }


type alias ThreadScript =
    { id : String
    , subject : String
    , first : Email
    , actions : List EmailResponse
    , scenes : Dict String ThreadScene 
    }
