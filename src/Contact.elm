module Contact exposing (..)

import ScriptTypes as Types


create : Types.AddressbookEntry
create =
    { email = "xxxxerror"
    , short = "xxxxerror"
    , full = "xxxxerror"
    }


email : String -> Types.AddressbookEntry -> Types.AddressbookEntry
email s entry =
    { entry | email = s }


short : String -> Types.AddressbookEntry -> Types.AddressbookEntry
short s entry =
    { entry | short = s }


full : String -> Types.AddressbookEntry -> Types.AddressbookEntry
full s entry =
    { entry | full = s }
