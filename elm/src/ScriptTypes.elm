module ScriptTypes exposing (Action(..), AddressbookEntry, Condition(..), Email, EmailResponse, ScriptComponent, Step, ThreadScript)


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


type alias ScriptComponent =
    { receivedEmail : Email
    , availableResponses : List EmailResponse
    }


type alias Step =
    { contents : ScriptComponent
    , guards : List Condition
    , key : Maybe String
    }


type alias ThreadScript =
    { subject : String
    , script : List Step
    }



