module App exposing (ActiveThread, ActiveThreadState(..), InboxState(..), Model, Msg(..), ThreadLocation)

{- Types used by the runtime. -}

import Browser
import Browser.Navigation
import Message exposing (Message)
import Script exposing (Script)
import Props exposing (Props)
import Url exposing (Url)


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
    | SetFlag { thread : String, key : String, value : Bool }
    | SetMaybeIntProp { thread : String, key : String, value : Maybe Int }
    | Select String Int
    | OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest


type InboxState
    = ThreadOpen { location : ThreadLocation }
    | InboxOpen


type alias ThreadLocation =
    { inboxIndex : Int
    , scriptId : String
    }


{-| ActiveThread props:

  - archivable (flag)
  - archived (flag)
  - size (int)
  - starred (flag)
  - important (flag)
  - importantSetByUser (flag)
  - unread (flag)
  - selection (maybe int)

-}
type alias ActiveThread =
    { scriptId : String
    , contents : List Message
    , state : ActiveThreadState
    , props : Props
    }


type ActiveThreadState
    = Waiting
    | Ready { responseOptions : List Script.EmailResponse }
