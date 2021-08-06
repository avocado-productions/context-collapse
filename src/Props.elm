module Props exposing (empty)

import Storage


{-| Set one specific place to decide what the empty props are
-}
empty : Storage.Storage
empty =
    Storage.emptyAbort Debug.todo
