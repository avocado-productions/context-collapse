module Example exposing (..)

import Script exposing (..)


addressBook : List AddressbookEntry
addressBook =
    [ { key = "dawn", email = "dawn@", short = "Dawn", full = "Dawn Hatton" }
    , { key = "felix", email = "felix@", short = "Felix", full = "Felix Tanenbaum" }
    , { key = "anuj", email = "anuj@", short = "Anuj", full = "Anuj Narayanan" }
    ]


exampleScript : ThreadScript
exampleScript =
    { subject = "Drinks tonight?"
    , script =
        [ { guards = []
          , key = Nothing -- UNNAMED script steps are root events
          , contents =
                { receivedEmail =
                    { from = "anuj"
                    , to = [ "dawn", "felix" ]

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
                            { from = "dawn"
                            , to = [ "anuj" ]
                            , contents = [ "Anuj, you know how much I hate Felix." ]
                            }
                      }
                    , { shortText = "No thanks."
                      , actions = [] -- No actions. In this case that means the thread is over.
                      , email =
                            { from = "dawn"
                            , to = [ "anuj" ]
                            , contents = [ "Sorry, I've got to meet up with my advisor. Say hi to Felix!" ]
                            }
                      }
                    , { shortText = "Sounds great."
                      , actions = [] -- No actions. In this case that means the thread is over.
                      , email =
                            { from = "dawn"
                            , to = [ "anuj", "felix" ]
                            , contents = [ "Sounds great! I can't wait to see you again. Felix, I hope you can make it too!" ]
                            }
                      }
                    ]
                }
          }
        , { key = Just "conflict" -- This thread is enabled by the (Goto "conflict") action
          , guards = [] -- No other guards. As soon as the email is enabled it can show up.
          , contents =
                { receivedEmail =
                    { from = "anuj"
                    , to = [ "dawn" ]
                    , contents = [ "Oh crap. I still forget.", "I'm really sorry!", " --A" ]
                    }
                , availableResponses =
                    [ { shortText = "Okay", actions = [], email = { from = "dawn", to = [ "anuj" ], contents = [ "It's okay." ] } }
                    , { shortText = "Hate this", actions = [], email = { from = "dawn", to = [ "anuj" ], contents = [ "You keep doing this!" ] } }
                    ]
                }
          }
        , { key = Just "conflict" -- Multiple script responses can have the same key, but only one will ever send (the first one whose guards match)
          , guards = [ Never ] -- This guard will never match, only useful for debugging or demonstration purposes
          , contents =
                { receivedEmail =
                    { from = "anuj"
                    , to = [ "dawn" ]
                    , contents = [ "What?" ]
                    }
                , availableResponses = []
                }
          }
        ]
    }
