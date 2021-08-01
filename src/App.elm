module App exposing (ActiveThread, ActiveThreadState(..), InboxState(..), Model, Msg(..), ThreadLocation)

{- Types used by the runtime. -}

import Browser
import Browser.Navigation
import Message exposing (Message)
import Url exposing (Url)
import Script exposing (Script)


type alias Model =
    { blocked : Maybe { scriptId : String, next : String }
    , script : Script
    , inbox : List ActiveThread
    , state : InboxState
    , navKey : Browser.Navigation.Key
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
    , contents : List Message
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
