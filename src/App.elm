module App exposing (ActiveThread, ActiveThreadState(..), Model, Msg(..), ViewMsg(..), emptyProps)

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
    , navKey : Browser.Navigation.Key
    , attachment : Maybe Props
    }


type Msg
    = DoNothing
    | NavPushUrl String
    | NavBack
    | OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
    | V ViewMsg


type ViewMsg
    = Refresh
    | OpenInbox
    | OpenThread { threadId : String }
    | Star { threadId : String, value : Bool }
    | Important { threadId : String, value : Bool }
    | Recommendation { threadId : String, value : Maybe Int }
    | Archive { threadId : String }
    | Select String Int
    | Attachment (Maybe Props)


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
