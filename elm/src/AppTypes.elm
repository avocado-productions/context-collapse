module AppTypes exposing (ActiveThread, ActiveThreadState(..), GlobalContext, Model, Msg(..), ThreadScript)

{- Stores types not needed for static authoring, but used for coordination between
   the different parts of the running program. Msg is used in both update and view,
   Model is used everywhere, etc.
-}

import Dict exposing (Dict)
import ScriptTypes as Script
import Set exposing (Set)


type Msg
    = ReturnToInbox Int
    | OpenThread Int
    | MakeDecision Int Script.EmailResponse
    | DoAction Int Script.Action
    | CheckForEnabled
    | ToggleSuggestion Int Int


type alias GlobalContext =
    { predicates : Set String
    }


type alias ThreadScript =
    { index : Int
    , subject : String
    , scenes : List Script.ThreadScene
    , enabled : Set String -- Email keys that could potentially be shown next, or that have been shown
    , used : Maybe (Set String) -- Just email keys that have been shown (Nothing if the thread hasn't started)
    }


type alias Model =
    { currentThread : Maybe Int
    , addressbook : Dict String Script.AddressbookEntry
    , you : String
    , context : GlobalContext
    , scripts : List ThreadScript
    , inbox : List ActiveThread
    }


type alias ActiveThread =
    { index : Int
    , subject : String
    , people : List String
    , contents : List Script.Email
    , state : ActiveThreadState
    }


type ActiveThreadState
    = Unread (List Script.EmailResponse) (Maybe Int)
    | Unresponded (List Script.EmailResponse) (Maybe Int)
    | Responded
