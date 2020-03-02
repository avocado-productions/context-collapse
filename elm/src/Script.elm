module Script exposing (Action, Condition, addressBook, exampleScript)


type alias AddressBookEntry =
    { key : String
    , email : String
    , short : String
    , full : String
    }


type alias Email =
    { from : String
    , to : List String
    , contents : List String
    }


type Action
    = Enable String


type Condition
    = Never


type alias EmailResponse =
    { shortText : String
    , email : Email
    , actions : List Action
    }


type alias ScriptComponent =
    { delivered : Email
    , options : List EmailResponse
    }


type alias Step =
    { contents : ScriptComponent
    , guards : List Condition
    , key : String
    }


type alias ThreadScript =
    { guards : List Condition
    , subject : String
    , start : ScriptComponent
    , script : List Step
    }


addressBook : List AddressBookEntry
addressBook =
    [ { key = "dawn", email = "dawn@", short = "Dawn", full = "Dawn Hatton" }
    , { key = "felix", email = "felix@", short = "Felix", full = "Felix Tanenbaum" }
    , { key = "anuj", email = "anuj@", short = "Anuj", full = "Anuj Narayanan" }
    ]


exampleScript : ThreadScript
exampleScript =
    { guards = [] -- This thread will trigger immediately
    , subject = "Drinks tonight?"
    , start =
        { delivered =
            { from = "anuj"
            , to = [ "dawn", "felix" ]

            {- Turn on text wrap for now o.O -}
            , contents =
                [ "My flight just landed. It'll probably be another 30 til I get to the hotel but then I was thinking of hitting the bar and finding a spot for dinner. Anyone else here and want to join?"
                , " -- Anuj"
                ]
            }
        , options =
            {- It's pretty redundant to have the author's name in the "from" field of every email
               but I feel like that could be treated as a default and modified if necessary (for instance,
               to allow the author to respond from a work vs. personal account or a sockpuppet.) Keeps
               the data structures simpler.
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
    , script =
        [ { key = "conflict" -- This thread is enabled by the (Goto "conflict") action
          , guards = [] -- No other guards. As soon as the email is enabled it can show up.
          , contents =
                { delivered = { from = "anuj", to = [ "dawn" ], contents = [ "Oh crap. I still forget.", "I'm really sorry!", " --A" ] }
                , options =
                    [ { shortText = "Okay", actions = [], email = { from = "dawn", to = [ "anuj" ], contents = [ "It's okay." ] } }
                    , { shortText = "Hate this", actions = [], email = { from = "dawn", to = [ "anuj" ], contents = [ "You keep doing this!" ] } }
                    ]
                }
          }
        , { key = "conflict" -- Multiple script responses can have the same key, but only one will ever send (the first one whose guards match)
          , guards = [ Never ] -- This guard will never match, only useful for debugging or demonstration purposes
          , contents = { delivered = { from = "anuj", to = [ "dawn" ], contents = [ "What?" ] }, options = [] }
          }
        ]
    }
