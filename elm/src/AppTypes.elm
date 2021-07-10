module AppTypes exposing (ActiveThread, ActiveThreadState(..), InboxState(..), Model, Msg(..), ThreadLocation)

{- Types used by the runtime. -}

import ScriptTypes as Script


type alias Model =
    { blocked : Maybe { scriptId : String, next : String }
    , scripts : List Script.ThreadScript
    , inbox : List ActiveThread
    , state : InboxState
    }


type Msg
    = ReturnToInbox
    | OpenThread ThreadLocation
    | ToggleSuggestion Int
    | SelectSuggestion
    | ArchiveThread


type InboxState
    = ThreadOpen { location : ThreadLocation }
    | InboxOpen


type alias ThreadLocation =
    { inboxIndex : Int
    , scriptId : String
    }


type alias ActiveThread =
    { scriptId : String
    , contents : List Script.Email
    , state : ActiveThreadState
    }


type ActiveThreadState
    = Responded { archivable : Bool }
    | Archived
    | Unread { archivable : Bool, responseOptions : List Script.EmailResponse }
    | Unresponded
        { archivable : Bool
        , responseOptions : List Script.EmailResponse
        , currentlySelectedOptionIndex : Maybe Int
        }
