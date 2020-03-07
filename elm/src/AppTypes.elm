module AppTypes exposing (ActiveThread, ActiveThreadState(..), Model, Msg(..), ScriptWithContext)

{- Stores types not needed for static authoring, but used for coordination between
   the different parts of the running program. Msg is used in both update and view,
   Model is used everywhere, etc.
-}

import Dict exposing (Dict)
import ScriptTypes as Script
import Set exposing (Set)


type Msg
    = ReturnToInbox Int
    | OpenThread ActiveThread
    | MakeDecision Int Script.EmailResponse
    | DoAction Int Script.Action
    | CheckForEnabled


type alias ScriptWithContext =
    { enabled : Set String -- Email keys that could potentially be shown next, or that have been shown
    , used : Maybe (Set String) -- Just email keys that have been shown (Nothing if the thread hasn't started)
    , script : Script.ThreadScript
    }


type alias Model =
    { currentThread : Maybe ActiveThread
    , addressbook : Dict String Script.AddressbookEntry
    , you : String
    , script : List ScriptWithContext
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
    = Unread (List Script.EmailResponse)
    | Unresponded (List Script.EmailResponse)
    | Responded
