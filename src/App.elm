module App exposing (ActiveThread, emptyProps, ActiveThreadState(..), InboxState(..), Model, Msg(..))

{- Types used by the runtime. -}

import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Message exposing (Message)
import Props exposing (Props)
import Props2 as Props
import Script exposing (Script)
import Url exposing (Url)


type alias Model =
    { blocked : List { threadId : String, next : String, delay : Int }
    , script : Script
    , threads : Dict String ActiveThread
    , inbox : List { threadId : String }
    , state : InboxState
    , navKey : Browser.Navigation.Key
    }


type Msg
    = DoNothing
    | NavPushUrl String
    | NavBack
    | OpenInbox
    | OpenThread { threadId : String }
    | Archive { threadId : String }
    | SetFlag { threadId : String, key : String, value : Bool }
    | SetMaybeIntProp { threadId : String, key : String, value : Maybe Int }
    | Select String Int
    | OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest


type InboxState
    = ThreadOpen { threadId : String }
    | InboxOpen


{-| ActiveThread props:

  - archivable (flag)
  - archived (flag)
  - size (int)
  - starred (flag)
  - important (flag)
  - importantSetByUser (flag)
  - unread (flag)
  - selection (maybe int)
  - open (flag)

-}
type alias ActiveThread =
    { threadId : String
    , contents : List Message
    , state : ActiveThreadState
    , props : Props
    }

emptyProps =
    Props.empty
        |> Props.expectFlag "archivable"
        |> Props.expectFlag "archived"
        |> Props.expectInt "size"
        |> Props.expectFlag "starred"
        |> Props.expectMaybeBool "important"
        |> Props.expectFlag "unread"
        |> Props.expectMaybeInt "selection"


type ActiveThreadState
    = Waiting
    | Ready { responseOptions : List Script.EmailResponse }
