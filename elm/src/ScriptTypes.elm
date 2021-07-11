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
    , next : Maybe String
    , spawn : List String
    }


type Action
    = Respond EmailResponse
    | Immediate String
    | Archive


type alias ThreadScene =
    { receivedEmail : Email
    , actions : List Action
    }


type alias ThreadScript =
    { id : String
    , subject : String
    , start : String
    , scenes : Dict String ThreadScene
    }
