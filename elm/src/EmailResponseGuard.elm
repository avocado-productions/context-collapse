module EmailResponseGuard exposing (..)

import ScriptTypes as Types


create : Types.EmailResponseGuard
create =
    { condition = Types.IsSet "xxxerror" -- TODO should be conditions
    , from = Nothing
    , message = Nothing
    }


isSet : String -> Types.EmailResponseGuard -> Types.EmailResponseGuard
isSet s entry =
    { entry | condition = Types.IsSet s }

isUnset : String -> Types.EmailResponseGuard -> Types.EmailResponseGuard
isUnset s entry =
    { entry | condition = Types.IsUnset s }

from : Types.AddressbookEntry -> Types.EmailResponseGuard -> Types.EmailResponseGuard
from s entry =
    { entry | from = Just s }


message : String -> Types.EmailResponseGuard -> Types.EmailResponseGuard
message s entry =
    { entry | message = Just s }
