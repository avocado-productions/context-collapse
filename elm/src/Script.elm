module Script exposing (myScript, you)

import ScriptTypes exposing (..)


confName : String
confName =
    "IRIS"


you : AddressbookEntry
you =
    naolin


naolin : AddressbookEntry
naolin =
    { email = "naolin@panopticorp.co", short = "Naolin", full = "Naolin Dawson Vega" }


dawn : AddressbookEntry
dawn =
    { email = "dawn@hometownpettsitters.com", short = "Dawn", full = "Dawn Hatton" }


cynthia : AddressbookEntry
cynthia =
    { email = "cynthia@panopticorp.co", short = "Cynthia", full = "Cynthia Cui" }


felix : AddressbookEntry
felix =
    { email = "felix@panopticorp.co", short = "Felix", full = "Felix Tanenbaum" }


anuj : AddressbookEntry
anuj =
    { email = "anuj@panopticorp.co", short = "Anuj", full = "Anuj Narayanan" }


conf : AddressbookEntry
conf =
    { email = "committee@irisconf.org", short = confName, full = confName ++ "Conference Organizers" }


david : AddressbookEntry
david =
    { email = "david@", short = "David", full = "David Sims, Ph.D." }


kendall : AddressbookEntry
kendall =
    { email = "kgraham@incorp.com", short = "Kendall", full = "Kendall Graham" }


figit : AddressbookEntry
figit =
    { email = "statsbot@figit.com", short = "Figit", full = "Figit, Inc." }


welcome : ThreadScript
welcome =
    { subject = "Welcome to " ++ confName ++ "!"
    , scenes =
        [ { guards = []
          , key = Nothing
          , actions = [ Set "welcome_in_inbox" ]
          , receivedEmail =
                { from = conf
                , to = [ conf ]
                , contents =
                    [ "Dear " ++ you.full ++ ","
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
        [ { guards = [ IsSet "welcome_in_inbox" ]
          , key = Nothing
          , actions = []
          , receivedEmail =
                { from = dawn
                , to = [ naolin ]
                , contents =
                    [ "Hi " ++ you.short
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


bossDrinksThread : ThreadScript
bossDrinksThread =
    { subject = "Anyone in town yet?"
    , scenes =
        [ { guards = [ IsSet "welcome_in_inbox" ]
          , key = Nothing -- UNNAMED script steps are root events
          , actions = []
          , receivedEmail =
                { from = anuj
                , to = [ naolin, felix, cynthia ]

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
                  , guards = []
                  , actions = [ Enable "conflict", Enable "FelixReply" ] -- This enables the "conflict" response(s) in this thread
                  , email =
                        { from = naolin
                        , to = [ anuj ]
                        , contents = [ "Anuj, you know Felix and I don't really get along." ]
                        }
                  }
                , { shortText = "No thanks."
                  , guards = []
                  , actions = [ Enable "FelixReply" ]
                  , email =
                        { from = naolin
                        , to = [ anuj ]
                        , contents = [ "Sorry, I've got to work on the slides for my talk. Have fun!" ]
                        }
                  }
                , { shortText = "Sounds great."
                  , guards = []
                  , actions = [ Set "hasPlans", Enable "AnujHappy", Enable "FelixReply" ]
                  , email =
                        { from = naolin
                        , to = [ anuj, felix, cynthia ]
                        , contents = [ "Sounds great! I can't wait to see everyone." ]
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
                [ { shortText = "Okay", guards = [], actions = [], email = { from = naolin, to = [ anuj ], contents = [ "It's okay." ] } }

                -- , { shortText = "Hate this", actions = [], email = { from = naolin, to = [ anuj ], contents = [ "You keep doing this!" ] } }
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
        , { key = Just "AnujHappy"
          , guards = []
          , actions = []
          , receivedEmail =
                { from = anuj
                , to = [ naolin ]
                , contents = [ "Hooray! See you soon.", "-- A" ]
                }
          , availableResponses =
                [ { shortText = "Actually..."
                  , guards = []
                  , actions = [ Unset "hasPlans" ]
                  , email = { from = naolin, to = [ anuj, felix, cynthia ], contents = [ "Sorry, something came up. I need to back out. Have fun!" ] }
                  }
                ]
          }
        , { key = Just "FelixReply"
          , guards = []
          , actions = [ Set "AnujThreadSeen" ]
          , receivedEmail =
                { from = felix
                , to = [ anuj, naolin, cynthia ]
                , contents = [ "Sounds great! I'll be there. Brought cards for after.", "- Felix" ]
                }
          , availableResponses =
                []
          }
        ]
    }


bossConnection : ThreadScript
bossConnection =
    { subject = "Drinks tonight"
    , scenes =
        [ { guards = [ IsSet "AnujThreadSeen" ]
          , key = Nothing
          , actions = []
          , receivedEmail =
                { from = david
                , to = [ naolin, kendall ]
                , contents =
                    [ "Hi " ++ you.short ++ ","
                    , "I was talking to Kendall about your work and he was really interested. "
                        ++ "There might be a potential client lead in store. Want to join us "
                        ++ "at Avarice Bar around 6 to talk shop?"
                    , "-- David"
                    ]
                }
          , availableResponses =
                [ { shortText = "I can't..."
                  , guards = []
                  , actions = [ Set "day0End" ]
                  , email =
                        { from = naolin
                        , to = [ david, kendall ]
                        , contents =
                            [ "I wish I could, but I already made plans. "
                                ++ "Maybe we can talk tomorrow at the first coffee break?"
                            ]
                        }
                  }
                , { shortText = "Of course!"
                  , guards = []
                  , actions = [ Set "day0End" ]
                  , email =
                        { from = naolin
                        , to = [ david, kendall ]
                        , contents =
                            [ "Dr. Graham, I'm such an admirer of your work! "
                                ++ "Of course! I'll see you there."
                            ]
                        }
                  }
                ]
          }
        ]
    }



{- type alias ThreadScene =
   { key : Maybe String
   , guards : List Condition
   , receivedEmail : Email
   , actions : List Action
   , availableResponses : List EmailResponse
   }
-}


deviceReport : ThreadScript
deviceReport =
    { subject = "Your Daily Summary from " ++ figit.short
    , scenes = [ deviceReportScene ]
    }


deviceReportScene : ThreadScene
deviceReportScene =
    { key = Nothing
    , guards = [ IsSet "day0End" ]
    , receivedEmail =
        { from = figit
        , to = [ you ]
        , contents =
            [ "Hi " ++ you.short ++ ","
            , "Here's how your Tuesday went:"
            , "Total steps: 9,300 (Goal: 10,000)"
            , "Max heart rate: 180 BPM"
            , "Activity level: slightly below normal"
            , "Keep up the good work!"
            , "--" ++ figit.full
            ]
        }
    , actions = []
    , availableResponses = []
    }


cynthiaPlea : ThreadScript
cynthiaPlea =
    { subject = "Hey..."
    , scenes = [ cynthiaPleaScene ]
    }


cynthiaPleaScene : ThreadScene
cynthiaPleaScene =
    { key = Nothing
    , guards = [ IsSet "day0End" ]
    , receivedEmail =
        { from = cynthia
        , to = [ dawn ]
        , contents =
            [ "Hey " ++ you.short ++ ","
            , "Do you have some time to talk today? I need some advice."
            , "- Cynthia"
            ]
        }
    , actions = []
    , availableResponses =
        [ { shortText = "I can't..."
          , guards = []
          , actions = []
          , email =
                { from = naolin
                , to = [ cynthia ]
                , contents =
                    [ "I wish I could, but I already made plans. "
                        ++ "Maybe we can talk tomorrow sometime?"
                    ]
                }
          }
        , { shortText = "Of course!"
          , guards = []
          , actions = []
          , email =
                { from = naolin
                , to = [ cynthia ]
                , contents =
                    [ "Of course, C, I'm here for you. "
                        ++ "Find me at the break."
                    ]
                }
          }
        ]
    }


myScript : List ThreadScript
myScript =
    [ bossDrinksThread
    , welcome
    , petCheckIn1
    , bossConnection
    , cynthiaPlea
    , deviceReport
    ]
