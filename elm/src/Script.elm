module Script exposing (myScript, you)

import Contact
import ScriptTypes as Types


you : Types.AddressbookEntry
you =
    naolin


naolin : Types.AddressbookEntry
naolin =
    Contact.create
        |> Contact.email "naolin@panopticorp.co"
        |> Contact.short "Naolin"
        |> Contact.full "Naolin Dawson Vega"


dawn : Types.AddressbookEntry
dawn =
    Contact.create
        |> Contact.email "dawn@hometownpettsitters.com"
        |> Contact.short "Dawn"
        |> Contact.full "Dawn Hatton"


cynthia : Types.AddressbookEntry
cynthia =
    Contact.create
        |> Contact.email "cynthia@panopticorp.co"
        |> Contact.short "Cynthia"
        |> Contact.full "Cynthia Cui"


felix : Types.AddressbookEntry
felix =
    Contact.create
        |> Contact.email "felix@panopticorp.co"
        |> Contact.short "Felix"
        |> Contact.full "Felix Tanenbaum"


anuj : Types.AddressbookEntry
anuj =
    Contact.create
        |> Contact.email "anuj@panopticorp.co"
        |> Contact.short "Anuj"
        |> Contact.full "Anuj Narayanan"


conf : Types.AddressbookEntry
conf =
    Contact.create
        |> Contact.email "committee@irisconf.org"
        |> Contact.short "IRIS"
        |> Contact.full "IRISConf Organizers"


david : Types.AddressbookEntry
david =
    Contact.create |> Contact.email "david@" |> Contact.short "David" |> Contact.full "David Sims, Ph.D."


kendall : Types.AddressbookEntry
kendall =
    Contact.create
        |> Contact.email "kgraham@incorp.com"
        |> Contact.short "Kendall"
        |> Contact.full "Kendall Graham"


figit : Types.AddressbookEntry
figit =
    Contact.create
        |> Contact.email "statsbot@figit.com"
        |> Contact.short "Figit"
        |> Contact.full "Figit, Inc."


myScript : List Types.ThreadScript
myScript =
    [ { subject = "Welcome to IRIS"
      , scenes =
            [ { key = Nothing
              , actions = [ Types.Set "iris_welcome_recv" ]
              , guards = []
              , receivedEmail =
                    { from = conf
                    , to = [ conf ]
                    , contents =
                        [ "Dear Naolin,"
                        , "Welcome to IRIS 2020! Below, you'll find key information about the next few  days."
                        , "**Program:** The program is available online."
                        , "**Registration:** Registration will be available Tuesday evening from 5-7pm and each morning,  8-8:30."
                        , "**Meals:** Breakfast will be served 8-8:30, lunch 12-1 each day of the conference. Dinner  is on your own. 2"
                        ]
                    }
              , availableResponses = []
              }
            ]
      }
    , { subject = "Anyone in town yet?"
      , scenes =
            [ { key = Nothing
              , actions = []
              , guards = [ Types.IsSet "iris_welcome_recv" ]
              , receivedEmail =
                    { from = anuj
                    , to = [ naolin, felix, cynthia ]
                    , contents =
                        [ "My flight just landed. It'll probably be another 30 til I get to the hotel but then I was thinking of hitting the bar and finding a spot for dinner. Anyone else here and want to join?"
                        , "— Anuj"
                        ]
                    }
              , availableResponses =
                    [ { shortText = "Ugh, Felix."
                      , guards = []
                      , actions =
                            [ Types.Enable "Anuj is sorry", Types.Enable "Felix replies", Types.Set "hasPlans" ]
                      , email =
                            { from = you
                            , to = [ anuj ]
                            , contents = [ "Anuj, you know Felix and I don't really get along" ]
                            }
                      }
                    , { shortText = "No thanks."
                      , guards = []
                      , actions = [ Types.Set "hasPlans", Types.Enable "Felix replies" ]
                      , email =
                            { from = you
                            , to = [ anuj, felix, cynthia ]
                            , contents = [ "Sorry, I've got to work on the slides for my talk. Have fun!" ]
                            }
                      }
                    , { shortText = "Sounds great."
                      , guards = []
                      , actions =
                            [ Types.Set "hasPlans"
                            , Types.Enable "Anuj is happy"
                            , Types.Enable "Felix replies"
                            ]
                      , email =
                            { from = you
                            , to = [ anuj, felix, cynthia ]
                            , contents = [ "Sounds great! I can't wait to see everyone." ]
                            }
                      }
                    ]
              }
            , { key = Just "Felix replies"
              , actions = [ Types.Set "felixReplied" ]
              , guards = []
              , receivedEmail =
                    { from = felix
                    , to = [ anuj ]
                    , contents = [ "Sounds great! I'll be there. Brought cards for after.", "- Felix" ]
                    }
              , availableResponses = []
              }
            , { key = Just "Anuj is sorry"
              , actions = []
              , guards = [ Types.IsSet "felixReplied" ]
              , receivedEmail =
                    { from = anuj
                    , to = [ naolin ]
                    , contents = [ "Oh crap. I still forget. I'm really sorry! —A" ]
                    }
              , availableResponses =
                    [ { shortText = "Okay..."
                      , guards = []
                      , actions = []
                      , email = { from = you, to = [ anuj ], contents = [ "It's okay." ] }
                      }
                    ]
              }
            , { key = Just "Anuj is happy"
              , actions = []
              , guards = [ Types.IsSet "felixReplied" ]
              , receivedEmail = { from = anuj, to = [ naolin ], contents = [ "Hooray! See you soon.", "—A" ] }
              , availableResponses =
                    [ { shortText = "Actually..."
                      , guards = [ { condition = Types.IsSet "conflictWithDrinks" } ]
                      , actions = []
                      , email =
                            { from = you
                            , to = [ anuj ]
                            , contents = [ "Sorry, something came up. I need to back out. Have fun!" ]
                            }
                      }
                    ]
              }
            ]
      }
    , { subject = "Evening Visit"
      , scenes =
            [ { key = Nothing
              , actions = []
              , guards = [ Types.IsSet "iris_welcome_recv", Types.IsSet "hasPlans" ]
              , receivedEmail =
                    { from = dawn
                    , to = [ naolin ]
                    , contents =
                        [ "Hi Naolin!"
                        , "Tunafish did SO WELL with her meds tonight. She greeted me right when I came in the door and ate her dinner, then joined me on the couch for some cuddles before her shot."
                        , "[[image: tunafish1.jpg]]"
                        , "[[image: tunafish2.jpg]]"
                        , "Have a great evening!"
                        , "— Dawn"
                        ]
                    }
              , availableResponses = []
              }
            ]
      }
    ]
