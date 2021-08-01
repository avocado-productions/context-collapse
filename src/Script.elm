module Script exposing (..)

import Dict exposing (Dict)
import Markup exposing (Markup)
import Message exposing (Message)


type alias AddressbookEntry =
    { email : String
    , short : String
    , full : String
    }


type alias EmailResponse =
    { shortText : Markup
    , email : Message
    , next : Maybe String
    , spawn : List String
    }


type Action
    = Respond EmailResponse
    | Immediate String
    | Archive


type alias ThreadScene =
    { receivedEmail : Message
    , actions : List Action
    }


type alias ThreadScript =
    { id : String
    , subject : String
    , start : String
    , scenes : Dict String ThreadScene
    }


type alias Script =
    { me : AddressbookEntry
    , addressBook : Dict String AddressbookEntry
    , threads : List ThreadScript
    , starting : List String
    }
