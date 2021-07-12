module Script exposing (..)

import Dict exposing (Dict)


type alias Contact =
    { id : String
    , email : String
    , short : String
    , full : String
    }


type alias Script =
    { me : String
    , characters : Dict String Contact
    , threads : List Thread
    , starting : List String
    }


type Action
    = Respond EmailResponse
    | Immediate String


type alias Thread =
    { subject : String
    , from : String
    , to : List String
    , paragraphsReversed : List String
    , actionsReversed : List Action
    }


type alias EmailResponse =
    {}
