module Inbox exposing (..)


type Condition
    = UserActions Int


type Event
    = SpawnThread { thread : String }
    | RunScene { thread : String, scene : String }


type alias Model =
    { blocked : List { event : Event, awaiting : List Condition }
    , threads : List Thread
    }


type alias Thread =
    {}


type Msg
    = ChangeProperty { thread : String, key : String, value : String }
