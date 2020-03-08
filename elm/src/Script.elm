module Script exposing (myScript, you)

import ScriptTypes exposing (..)


confName : String
confName =
    "IRIS"

you : AddressbookEntry
you = naolin

naolin : AddressbookEntry
naolin = {email = "naolin@", short = "Naolin", full = "Naolin Johnson Vega"}

dawn : AddressbookEntry
dawn = {email = "dawn@hometownpettsitters.co", short = "Dawn", full = "Dawn Hatton" }


felix : AddressbookEntry
felix = {email = "felix@", short = "Felix", full = "Felix Tanenbaum" }

anuj : AddressbookEntry
anuj = {email = "anuj@", short = "Anuj", full = "Anuj Narayanan"}

conf : AddressbookEntry
conf = {email = "committee@conf.org", short = confName, full = confName ++ "Conference Organizers"}



welcome : ThreadScript
welcome = { subject = "Welcome to " ++ confName ++ "!"
    , scenes =
        [ { guards = []
          , key = Nothing
          , actions = [ Set "welcome_in_inbox" ]
          , receivedEmail =
                { from = conf
                , to = [ conf ]
                , contents = [ 
                        "Dear " ++ you.full ++ ","
                      , "Welcome to " ++ confName ++ "2020! Below, you'll find key information about the next few days."
                      , "Program:"
                      , "The program is available online."
                      , "Registration:"
                      , "Registration will be available Tuesday evening from 5-7pm and each morning, 8-8:30."
                      , "Meals:"
                      , "Breakfast will be served 8-8:30, lunch 12-1 each day of the conference. Dinner is on your own."
                ]
                }
          , availableResponses = []
          }
        ]
    }

petCheckIn1 : ThreadScript
petCheckIn1 = 
      { subject = "Evening visit"
      , scenes = 
            [ {
                  guards = [ IsSet "welcome_in_inbox"]
                  , key = Nothing
                  , actions = []
                  , receivedEmail = {
                        from = dawn,
                        to = [naolin],
                        contents = ["Hi "++you.short
                        , "Tunafish did SO WELL with her meds tonight. She greeted me right when I came in the door and ate her dinner, then joined me on the couch for some cuddles before her shot."
                        , "[IMG, IMG]"
                        , "Have a great evening!"
                        , "-- Dawn"
                        ]
                  }
                  , availableResponses = []
             }
            ]
      }

drinksScene : ThreadScript
drinksScene =
    { subject = "Drinks tonight?"
    , scenes =
        [ { guards = [ IsSet "welcome_in_inbox"]
          , key = Nothing -- UNNAMED script steps are root events
          , actions = []
          , receivedEmail =
                { from = anuj
                , to = [ naolin, felix ]

                {- Turn on text wrap for now o.O -}
                , contents =
                    [ "My flight just landed. It'll probably be another 30 til I get to the hotel but then I was thinking of hitting the bar and finding a spot for dinner. Anyone else here and want to join?"
                    , " -- Anuj"
                    ]
                }
          , availableResponses =
                {- It's pretty redundant to have the author's name in the "from" field of every email
                   but it keeps the data structures simpler. (Also allows sockpuppets?)
                -}
                [ { shortText = "Ugh, Felix."
                  , actions = [ Enable "conflict" ] -- This enables the "conflict" response(s) in this thread
                  , email =
                        { from = naolin
                        , to = [ anuj ]
                        , contents = [ "Anuj, you know Felix and I don't really get along." ]
                        }
                  }
                , { shortText = "No thanks."
                  , actions = []
                  , email =
                        { from = naolin
                        , to = [ anuj ]
                        , contents = [ "Sorry, I've got to meet up with my advisor. Say hi to Felix!" ]
                        }
                  }
                , { shortText = "Sounds great."
                  , actions = [ Set "has_plans" ]
                  , email =
                        { from = naolin
                        , to = [ anuj, felix ]
                        , contents = [ "Sounds great! I can't wait to see you again. Felix, I hope you can make it too!" ]
                        }
                  }
                ]
          }
        , { key = Just "conflict" -- This thread is enabled by the (Goto "conflict") action
          , guards = [ IsUnset "something_that_never_gets_set" ] -- This will be always true. As soon as the email is enabled it can show up.
          , actions = []
          , receivedEmail =
                { from = anuj
                , to = [ naolin ]
                , contents = [ "Oh crap. I still forget.", "I'm really sorry!", " --A" ]
                }
          , availableResponses =
                [ { shortText = "Okay", actions = [], email = { from = naolin, to = [ anuj ], contents = [ "It's okay." ] } }
                , { shortText = "Hate this", actions = [], email = { from = naolin, to = [ anuj ], contents = [ "You keep doing this!" ] } }
                ]
          }
        , { key = Just "conflict" -- Multiple script responses can have the same key, but only one will ever send (the first one whose guards match)
          , guards = [ IsSet "something_that_never_gets_set" ] -- This guard will never match
          , actions = []
          , receivedEmail =
                { from = anuj
                , to = [ naolin ]
                , contents = [ "What?" ]
                }
          , availableResponses = []
          }
        ]
    }


myScript : List ThreadScript
myScript =
    [ drinksScene
    , welcome
    , petCheckIn1
    ]
