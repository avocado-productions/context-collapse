module Script exposing (me, myScript, starting)

import Contact
import Dict
import ScriptTypes as Types


me : Types.AddressbookEntry
me =
    Contact.create
        |> Contact.email "naolin@rivendel.edu"
        |> Contact.short "Naolin"
        |> Contact.full "Naolin Dawson Vega"

sam : Types.AddressbookEntry
sam =
    Contact.create
        |> Contact.email "coolsam@gmail.com"
        |> Contact.short "Sam"
        |> Contact.full "Samantha Quimby"

erik : Types.AddressbookEntry
erik =
    Contact.create
        |> Contact.email "dragonfire3489@gmail.com"
        |> Contact.short "Erik"
        |> Contact.full "Erik Jaffe"

dslist : Types.AddressbookEntry
dslist =
    Contact.create
        |> Contact.email "ds5.social@googlegroups.com"
        |> Contact.short "DS 5"
        |> Contact.full "The DS 5 Mailing List"


advisor : Types.AddressbookEntry
advisor =
    Contact.create
        |> Contact.email "wzhou@rivendel.edu"
        |> Contact.short "Wei Zhou"
        |> Contact.full "Prof. Wei Zhou"


college_friend : Types.AddressbookEntry
college_friend =
    Contact.create
        |> Contact.email "christine@upprcut.com"
        |> Contact.short "Christine"
        |> Contact.full "Christine Malcolm"


ds : Types.AddressbookEntry
ds =
    Contact.create
        |> Contact.email "info@downtownsouth.com"
        |> Contact.short "Downtown"
        |> Contact.full "Downtown South Bar & Grille"


apero : Types.AddressbookEntry
apero =
    Contact.create
        |> Contact.email "cheers@aperodining.com"
        |> Contact.short "Apero"
        |> Contact.full "Apero"


drinkupapp : Types.AddressbookEntry
drinkupapp =
    Contact.create
        |> Contact.email "email@drinkitallup.com"
        |> Contact.short "Drink"
        |> Contact.full "Drink it All Up!"


starting =
    [ "convo-ds", "convo-b", "convo-c" ]


myScript : List Types.ThreadScript
myScript =
    [ { id = "convo-ds"
      , subject = "Drinks tonight?"
      , start = "a-zero"
      , scenes =
            Dict.fromList
                [ ( "a-zero"
                  , { receivedEmail =
                        { from = dslist
                        , to = [ me ]
                        , contents = [ "Who wants to grab a drink at the DS tonight?" ]
                        }
                    , actions =
                        [ Types.Respond
                            { shortText = "Meet somewhere closer?"
                            , email =
                                { from = me
                                , to = [ dslist ]
                                , contents = [ """Could we meet somewhere closer to my
                    apartment? There's a new bar called Apero that just
                    opened up down the street.""" ]
                                }
                            , next = Just "a-one"
                            , spawn = [ "apero-spam" ]
                            }
                        , Types.Respond
                            { shortText = "I'm in"
                            , email =
                                { from = me
                                , to = [ dslist ]
                                , contents = [ "Save me a seat!" ]
                                }
                            , next = Just "a-one"
                            , spawn = [ "ds-spam", "app-spam" ]
                            }
                        ]
                    }
                  )
                , ( "a-one"
                  , { receivedEmail =
                        { from = erik
                        , to = [ me, dslist ]
                        , contents = [ 
                          """ I'd be up for trying something new! Standard
                          meeting time?
                            - Edog """ ]
                        }
                    , actions =
                        [ Types.Respond
                            { shortText = "Yeah, 7:30"
                            , email =
                                { from = me
                                , to = [ dslist ]
                                , contents = [ "Sure, see you at 7:30" ]
                                }
                            , next = Just "a-two"
                            , spawn = []
                            }
                        , Types.Respond
                            { shortText = "Later, 9pm"
                            , email =
                                { from = me
                                , to = [ erik, dslist ]
                                , contents = [ """Um actually I need to
                                finish something for my advisor first, so I
                                can't be there til 9pm. The rest of you can meet
                                earlier though.""" ]
                                }
                            , next = Just "a-three"
                            , spawn = []
                            }
                        ]
                    }
                  )
                , ( "a-two"
                  , { receivedEmail =
                        { from = sam
                        , to = [ dslist, me ]
                        , contents = [ "That was fun everyone!" ]
                        }
                    , actions =
                        []
                    }
                  )
                , ( "a-three"
                  , { receivedEmail =
                        { from = sam
                        , to = [ me ]
                        , contents = [ """You make it home ok?? You were
                        pretty out of it.""" ]
                        }
                    , actions =
                        [ Types.Immediate "a-four" ]
                    }
                  )
                , ( "a-four"
                  , { receivedEmail =
                        { from = dslist
                        , to = [ me ]
                        , contents = [ "You were pretty out of it, continued" ]
                        }
                    , actions =
                        [ Types.Immediate "a-five" ]
                    }
                  )
                , ( "a-five"
                  , { receivedEmail =
                        { from = dslist
                        , to = [ me ]
                        , contents = [ "Bad ending" ]
                        }
                    , actions =
                        []
                    }
                  )
                ]
      }
    , { id = "convo-b"
      , subject = "Your thesis progress"
      , start = "b-1"
      , scenes =
            Dict.fromList
                [ ( "b-1"
                  , { receivedEmail =
                        { from = advisor
                        , to = [ me ]
                        , contents =
                            [ "Naolin,"
                            , """I had some questions about this last chapter you sent
                me. I couldn't find the `Lemma 4.8' you referenced in your
                proof of cut elimination, and it seems critical to making
                the whole thing hang together. Actually, I'm not sure your
                logic admits cut at all. Please get back to me to address
                this urgent matter."""
                            , "Prof. Zhou"
                            ]
                        }
                    , actions =
                        [ Types.Respond
                            { shortText = "Haven't proved it yet"
                            , email =
                                { from = me
                                , to = [ advisor ]
                                , contents =
                                    [ "Prof. Zhou,"
                                    , """Oh, thanks for catching that! Yeah, I hadn't
                      proved that lemma yet. I was working on it but I got
                      stuck on the commutative case. Maybe we could meet to
                      talk it through."""
                                    ]
                                }
                            , next = Just "b-2"
                            , spawn = []
                            }
                        ]
                    }
                  )
                , ( "b-2"
                  , { receivedEmail =
                        { from = advisor
                        , to = [ me ]
                        , contents =
                            [ "Naolin,"
                            , """Don't waste your time. I found a
                          counterexample to the cut elimination theorem
                          (attached).
                          The issue is with your side condition in the
                          ELT-E rule. Remind me why you needed that side
                          condition again?"""
                            , "Zhou"
                            ]
                        }
                    , actions =
                        [ Types.Respond
                            { shortText = "It's not that important"
                            , email =
                                { from = me
                                , to = [ advisor ]
                                , contents = [ 
                                  """Well, hmm. I just worked out all my
                                    benchmark examples without it, so I
                                    can't really remember why I needed it.
                                    It might have been to handle some
                                    corner case that disappeared with the
                                    2-context redesign. Does the proof go
                                    through without it?""" 
                                  ]
                                }
                            , next = Just "b-3"
                            , spawn = []
                            },
                          Types.Respond
                            { shortText = "It's really important"
                            , email =
                                { from = me
                                , to = [ advisor ]
                                , contents = [ 
                                    """Oh, wait, I remember... without that
                                    side condition, the whole logic is
                                    degenerate. It lets you prove A from
                                    \\circ{A}, basically, so the monad does
                                    nothing at all. So, okay, we really
                                    need to meet and talk this through. Are
                                    you available tomorrow morning? I can
                                    make it to campus early if need be."""
                                    ]
                                }
                            , next = Just "b-4"
                            , spawn = []
                            }
                        ]
                    }
                  )
                , ( "b-3"
                  , { receivedEmail =
                        { from = advisor
                        , to = [ me ]
                        , contents = [ "Third email" ]
                        }
                    , actions =
                        [ Types.Respond
                            { shortText = "Respond"
                            , email =
                                { from = me
                                , to = [ advisor ]
                                , contents = [ "Third response" ]
                                }
                            , next = Just "b-4"
                            , spawn = []
                            }
                        ]
                    }
                  )
                , ( "b-4"
                  , { receivedEmail =
                        { from = advisor
                        , to = [ me ]
                        , contents = [ "Fourth email" ]
                        }
                    , actions =
                        []
                    }
                  )
                ]
      }
    , { id = "convo-c"
      , subject = "Jobs at my company"
      , start = "c-1"
      , scenes =
            Dict.fromList
                [ ( "c-1"
                  , { receivedEmail =
                        { from = college_friend
                        , to = [ me ]
                        , contents = [ "Hey " ++ me.short ++ """! Can't wait to see you when
            you get into town! 

            Also, wanted to let you know Panoptico is hiring research
            software engineers in machine learning. When are you graduating
            again? It'd be so awesome if you came and worked here. Let me
            know if you're interested and I'll talk to my boss.""" ]
                        }
                    , actions =
                        [ Types.Respond
                            { shortText = "Also looking forward"
                            , email =
                                { from = me
                                , to = [ college_friend ]
                                , contents = [ """I'm looking forward to it too boo.
                    Haha, don't you know never to ask a grad student when
                    they're finishing their thesis? But honestly I could
                    bail if a good job comes along. Tell me more?""" ]
                                }
                            , next = Just "c-2"
                            , spawn = []
                            }
                        ]
                    }
                  )
                , ( "c-2"
                  , { receivedEmail =
                        { from = college_friend
                        , to = [ me ]
                        , contents = [ "Second email" ]
                        }
                    , actions =
                        [ Types.Respond
                            { shortText = "Respond"
                            , email =
                                { from = me
                                , to = [ college_friend ]
                                , contents = [ "Second response" ]
                                }
                            , next = Just "c-3"
                            , spawn = []
                            }
                        ]
                    }
                  )
                , ( "c-3"
                  , { receivedEmail =
                        { from = college_friend
                        , to = [ me ]
                        , contents = [ "Third email" ]
                        }
                    , actions =
                        [ Types.Respond
                            { shortText = "Respond"
                            , email =
                                { from = me
                                , to = [ college_friend ]
                                , contents = [ "Third response" ]
                                }
                            , next = Just "c-4"
                            , spawn = []
                            }
                        ]
                    }
                  )
                , ( "c-4"
                  , { receivedEmail =
                        { from = college_friend
                        , to = [ me ]
                        , contents = [ "Fourth email" ]
                        }
                    , actions =
                        []
                    }
                  )
                ]
      }
    , { id = "ds-spam"
      , subject = "Downtown South is open!"
      , start = "start"
      , scenes =
            Dict.fromList
                [ ( "start"
                  , { receivedEmail =
                        { from = ds
                        , to = [ me ]
                        , contents =
                            [ "Downtown South is open!"
                            , """After extenisve repairs, the Downtown South Bar & Grille
          is back open for business. In honor of our flood damage,
          we've got a disaster-themed menu for you all tonight!"""
                            , """Flood the fires of your appetite with a nice cold drink and let
          Wet Willie do the cooking: enjoy Drenched Nachos, (Advancing)
          Surf and (Retreating) Turf, or try out our brand-new patented
          Storm Sturgeon Salad."""
                            , """"""
                            , """Hurry in before the next full moon floods us again!"""
                            , """— Wet Willie from the Grille"""
                            ]
                        }
                    , actions = [ Types.Archive ]
                    }
                  )
                ]
      }
    , { id = "apero-spam"
      , subject = "Apero Grand Opening: Next Thursday"
      , start = "start"
      , scenes =
            Dict.fromList
                [ ( "start"
                  , { receivedEmail =
                        { from = apero
                        , to = [ me ]
                        , contents =
                            [ """Dear loyal early customer!"""
                            , """Here at Apero, we're been excited by the early response 
                we've gotten during our "soft-open" period this month. We wanted to
                let all of our first fans know that our official Grand Opening party
                is happening soon: next Thursday we'll not just be open, but we'll
                be open with bells on!"""
                            , """Come on down in your fanciest tuxedo (or your nicest t-shirt and
                ripped jeans) for
                drink specials, 3-for-1 appetizers, and a grand old time at our grand
                opening. Or come on by sooner: we're exited to see you anytime, and
                you've clearly figured out that we're here without a grand opening!"""
                            , """"""
                            , """Your favorite terribly-kept neighborhood boozy secret,"""
                            , """Apero Staff"""
                            ]
                        }
                    , actions = [ Types.Archive ]
                    }
                  )
                ]
      }
    , { id = "app-spam"
      , subject = "Thanks for downloading Drink.up"
      , start = "start"
      , scenes =
            Dict.fromList
                [ ( "start"
                  , { receivedEmail =
                        { from = drinkupapp
                        , to = [ me ]
                        , contents =
                            [ """Welcome to the thirstiest and tastiest family you'll
                ever be a part of... unless you've got a really cool family, we guess!"""
                            , """Remember to use the Drink.up app to check in whenever you have a 
                new beer, some fancy soda... or even when you grab some tap water and
                chug it down!"""
                            , """For every liter of beverage you track on the Drink.up app, you'll
                get points towards free food and travel. Why? Because We've Got Venture
                Capitol Money, That's Why™"""
                            , """"""
                            , """Your friends at Team Drink.up"""
                            , """https://drink.up/"""
                            ]
                        }
                    , actions = [ Types.Archive ]
                    }
                  )
                ]
      }
    ]
