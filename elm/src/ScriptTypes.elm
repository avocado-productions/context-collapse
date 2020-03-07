module ScriptTypes exposing (Action(..), AddressbookEntry, Condition(..), Email, EmailResponse, ThreadScene, ThreadScript)


type alias AddressbookEntry =
    { key : String
    , email : String
    , short : String
    , full : String
    }


type alias Email =
    { from : String
    , to : List String
    , contents : List String
    }


type Action
    = Enable String


type Condition
    = Never


type alias EmailResponse =
    { shortText : String
    , email : Email
    , actions : List Action
    }


type alias ThreadScene =
    { key : Maybe String
    , guards : List Condition
    , receivedEmail : Email
    , availableResponses : List EmailResponse
    }


type alias ThreadScript =
    { subject : String
    , scenes : List ThreadScene
    }
