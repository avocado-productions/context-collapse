module App exposing (ActiveThread, ActiveThreadState(..), InboxState(..), Model, Msg(..), ThreadLocation)

{- Types used by the runtime. -}

import Browser
import Browser.Navigation
import ScriptTypes as Script
import Url exposing (Url)


type alias Model =
    { blocked : Maybe { scriptId : String, next : String }
    , scripts : List Script.ThreadScript
    , inbox : List ActiveThread
    , state : InboxState
    , navKey : Browser.Navigation.Key
    , me : Script.AddressbookEntry
    }


type Msg
    = DoNothing
    | NavPushUrl String
    | NavBack
    | OpenInbox
    | OpenThread ThreadLocation
    | ToggleStar String
    | ToggleSuggestion Int
    | SelectSuggestion
    | ArchiveThread
    | OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest


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
    , starred : Bool
    , size : Int
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
