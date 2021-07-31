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


savealltime : Types.AddressbookEntry
savealltime =
    Contact.create
        |> Contact.email "save@allthe.time"
        |> Contact.short "Save"
        |> Contact.full "Time Saver X"


starting =
    [ "thingamajig" ]


myScript : List Types.ThreadScript
myScript =
    [ { id = "thingamajig"
      , subject = "Inbox Zero Success!"
      , start = "foo"
      , scenes =
            Dict.fromList
                [ ( "foo"
                  , { receivedEmail =
                        { from = savealltime
                        , to = [ me ]
                        , contents =
                            [ "Happy Friday!"
                            , "You successfully reached Inbox Zero yesterday!"
                            , "Good luck today - get that to-do list down!"
                            , "Your friends at TSX"
                            , ""
                            , "Get it done with Time Saver X"
                            ]
                        }
                    , actions =
                        [ Types.Respond
                            { shortText = "UNSUBSCRIBE "
                            , email = { from = me, to = [ savealltime ], contents = [ "UNSUBSCRIBE", "", "Also, fuck you guys. Who sends an email saying that you reached inbox zero?" ] }
                            , next = Nothing
                            , spawn = [ "convo-ds", "convo-b", "convo-c" ]
                            }
                        ]
                    }
                  )
                ]
      }
    , { id = "convo-ds"
      , subject = "Drinks tonight?"
      , start = "a-zero"
      , scenes =
            Dict.fromList
                [ ( "a-zero"
                  , { receivedEmail =
                        { from = erik
                        , to = [ dslist ]
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
                            , next = Just "a-two"
                            , spawn = [ "ds-spam", "app-spam" ]
                            }
                        ]
                    }
                  )
                , ( "a-one"
                  , { receivedEmail =
                        { from = erik
                        , to = [ me, dslist ]
                        , contents =
                            [ """ I'd be up for trying something new! Standard
                          meeting time?
                            - Edog """
                            ]
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
                            , next = Just "a-sam-in"
                            , spawn = []
                            }
                        ]
                    }
                  )
                , ( "a-sam-in"
                  , { receivedEmail =
                        { from = sam
                        , to = [ dslist, me ]
                        , contents = [ """Oh if we're not meeting til 9 I can
                        totally be there ^.^""" ]
                        }
                    , actions = [ Types.Archive ] -- XXX - continue with a-three
                    }
                  )
                , ( "a-two"
                  , { receivedEmail =
                        { from = erik
                        , to = [ dslist, me ]
                        , contents = [ "Got a table in the back!" ]
                        }

                    -- , actions = [ Types.Archive ]
                    , actions = [ Types.Immediate "sam-cant-make-it" ]
                    }
                  )
                , ( "sam-cant-make-it"
                  , { receivedEmail =
                        { from = sam
                        , to = [ dslist, me ]
                        , contents = [ "sry can't make it :(" ]
                        }
                    , actions = [ Types.Archive ]
                    }
                  )
                , ( "a-three"
                  , { receivedEmail =
                        { from = sam
                        , to = [ me ]
                        , contents = [ """You make it home ok?? You were
                        pretty out of it.""" ]
                        }
                    , actions = [ Types.Archive ] -- XXX continue
                    }
                  )
                ]

      --- End of DS thread emails
      }

    --- End of DS thread convo
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
                                , contents =
                                    [ """Well, hmm. I just worked out all my
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
                            }
                        , Types.Respond
                            { shortText = "It's really important"
                            , email =
                                { from = me
                                , to = [ advisor ]
                                , contents =
                                    [ """Oh, wait, I remember... without that
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
                        , contents = [ """
                          That's your job to figure out. If it doesn't
                          work, we may need to reschedule your defense next
                          month. Can you please work on the proof tonight
                          and let me know the result?
                          """ ]
                        }
                    , actions =
                        [ Types.Respond
                            { shortText = "*sigh* So much for socializing."
                            , email =
                                { from = me
                                , to = [ advisor ]
                                , contents =
                                    [ """Alright, sure. Of course. 
                                  I'll let you know as soon as I can."""
                                    ]
                                }
                            , next = Just "b-5"
                            , spawn = []
                            }
                        , Types.Respond
                            { shortText = "No, sorry"
                            , email =
                                { from = me
                                , to = [ advisor ]
                                , contents =
                                    [ """I can't tonight. Could we meet tomorrow
                                morning and talk it through?"""
                                    ]
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
                        , contents = [ """
                          Okay. I will be busy starting 9am but if you get
                          here at 8 I will have time for you. See you at 8
                          sharp.
                          """ ]
                        }
                    , actions =
                        [ Types.Archive ]
                    }
                  )
                , ( "b-5"
                  , { receivedEmail =
                        { from = advisor
                        , to = [ me ]
                        , contents = [ """Great. Thx""" ]
                        }
                    , actions = [ Types.Archive ]
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
                                , contents =
                                    [ "I'm looking forward to it too, boo!"
                                    , """Uhhhh <_< don't you know never to ask a grad student when
                                    they're finishing their thesis? ^_^;"""
                                    , """I guess it's going alright. Honestly, my advisor
                                    really stresses me out, and sometimes I just want to
                                    do a job where I can make progress every day and see
                                    more immediate gratification, you
                                    know?"""
                                    , "Talk soon,"
                                    , "Naolin"
                                    ]
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
                        , contents = [ """Yeah I hear you. Well, I mean, want
                            me to bring up your name to our recruiting
                            team?""" ]
                        }
                    , actions =
                        [ Types.Respond
                            { shortText = "Sure, why not"
                            , email =
                                { from = me
                                , to = [ college_friend ]
                                , contents = [ """You know what... sure. Why not? It
                                    can't hurt to give myself some options.
                                    Here's my CV (attached).""" ]
                                }
                            , next = Just "c-3"
                            , spawn = []
                            }
                        , Types.Respond
                            { shortText = "Nah"
                            , email =
                                { from = me
                                , to = [ college_friend ]
                                , contents = [ """No thanks. I want to focus
                                  on finishing up and probably apply for
                                  faculty jobs first, as unlikely as it
                                  seems that I'll ever get one lolsob...
                                  but I'll let you know if that doesn't pan
                                      out.""" ]
                                }
                            , next = Just "c-4"
                            , spawn = []
                            }
                        ]

                    -- End response options for c-2
                    }
                  )
                , ( "c-3"
                  , { receivedEmail =
                        { from = college_friend
                        , to = [ me ]
                        , contents = [ "Alright fair enough. See ya soon!" ]
                        }
                    , actions = [ Types.Archive ]
                    }
                  )
                , ( "c-4"
                  , { receivedEmail =
                        { from = college_friend
                        , to = [ me ]
                        , contents = [ "Awesome I'll pass this on! Thanks!" ]
                        }
                    , actions = [ Types.Archive ]
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
