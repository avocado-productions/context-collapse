module ScriptTypes exposing (Action(..), AddressbookEntry, Condition(..), Email, EmailResponse, EmailResponseGuard, ThreadScene, ThreadScript)


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


type Action
    = Enable String
    | Set String
    | Unset String


type Condition
    = IsSet String
    | IsUnset String
    | IsResponded String String


type alias EmailResponseGuard =
    { condition : Condition
    , from : Maybe AddressbookEntry
    , message : Maybe String
    }


type alias EmailResponse =
    { guards : List EmailResponseGuard
    , shortText : String
    , email : Email
    , actions : List Action
    }


type alias ThreadScene =
    { key : Maybe String
    , guards : List Condition
    , receivedEmail : Email
    , actions : List Action
    , availableResponses : List EmailResponse
    }


type alias ThreadScript =
    { subject : String
    , scenes : List ThreadScene
    }
