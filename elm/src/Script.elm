module Script exposing (me, myScript)

import Contact
import Dict
import ScriptTypes as Types


me : Types.AddressbookEntry
me =
    Contact.create
        |> Contact.email "naolin@panopticorp.co"
        |> Contact.short "Naolin"
        |> Contact.full "Naolin Dawson Vega"


dslist : Types.AddressbookEntry
dslist =
    Contact.create
        |> Contact.email "ds5.social@googlegroups.com"
        |> Contact.short "DS 5"
        |> Contact.full "The DS 5 Mailing List"


advisor : Types.AddressbookEntry
advisor =
    Contact.create
        |> Contact.email "wzhou@uni.edu"
        |> Contact.short "Wei Zhou"
        |> Contact.full "Prof. Wei Zhou"


college_friend : Types.AddressbookEntry
college_friend =
    Contact.create
        |> Contact.email "christine@upprcut.com"
        |> Contact.short "C. Malcolm"
        |> Contact.full "Christine Malcolm"


myScript : List Types.ThreadScript
myScript =
    [ { id = "convo-ds"
      , subject = "Drinks tonight?"
      , first =
            { from = dslist
            , to = [ me ]
            , contents = [ "Who wants to grab a drink at the DS tonight?" ]
            }
      , actions =
            [ { shortText = "Meet somewhere closer?"
              , email =
                    { from = me
                    , to = [ dslist ]
                    , contents = [ """Could we meet somewhere closer to my
                    apartment? There's a new bar called Apero that just
                    opened up down the street.""" ]
                    }
              , next = "a-one"
              }
            , { shortText = "I'm in"
              , email =
                    { from = me
                    , to = [ dslist ]
                    , contents = [ "Save me a seat!" ]
                    }
              , next = "a-one"
              }
            ]
      , scenes =
            Dict.fromList
                [ ( "a-one"
                  , { receivedEmail =
                        { from = dslist
                        , to = [ me ]
                        , contents = [ "Response from DS 5" ]
                        }
                    , actions =
                        [ Types.Respond
                            { shortText = "Go Left"
                            , email =
                                { from = me
                                , to = [ dslist ]
                                , contents = [ "Go Left" ]
                                }
                            , next = "a-two"
                            }
                        , Types.Respond
                            { shortText = "Go Right"
                            , email =
                                { from = me
                                , to = [ dslist ]
                                , contents = [ "Go Right" ]
                                }
                            , next = "a-three"
                            }
                        ]
                    }
                  )
                , ( "a-two"
                  , { receivedEmail =
                        { from = dslist
                        , to = [ me ]
                        , contents = [ "Good ending" ]
                        }
                    , actions =
                        []
                    }
                  )
                , ( "a-three"
                  , { receivedEmail =
                        { from = dslist
                        , to = [ me ]
                        , contents = [ "Bad ending starts" ]
                        }
                    , actions =
                        [ Types.Immediate "a-four" ]
                    }
                  )
                , ( "a-four"
                  , { receivedEmail =
                        { from = dslist
                        , to = [ me ]
                        , contents = [ "Bad ending continues" ]
                        }
                    , actions =
                        [ Types.Immediate "a-five" ]
                    }
                  )
                , ( "a-five"
                  , { receivedEmail =
                        { from = dslist
                        , to = [ me ]
                        , contents = [ "Bad ending ends" ]
                        }
                    , actions =
                        []
                    }
                  )
                ]
      }
    , { id = "convo-b"
      , subject = "Your thesis progress"
      , first =
            { from = advisor
            , to = [ me ]
            , contents = [ "Naolin,",
                """I had some questions about this last chapter you sent
                me. I couldn't find the `Lemma 4.8' you referenced in your
                proof of cut elimination, and it seems critical to making
                the whole thing hang together. Actually, I'm not sure your
                logic admits cut at all. Please get back to me to address
                this urgent matter.""",
                "Prof. Zhou"
            ]
            }
      , actions =
            [ { shortText = "Haven't proved it yet"
              , email =
                    { from = me
                    , to = [ advisor ]
                    , contents = [ "Prof. Zhou,",
                      """Oh, thanks for catching that! Yeah, I hadn't
                      proved that lemma yet. I was working on it but I got
                      stuck on the commutative case. Maybe we could meet to
                      talk it through."""
                    ]
                    }
              , next = "b-2"
              }
            ]
      , scenes =
            Dict.fromList
                [ ( "b-2"
                  , { receivedEmail =
                        { from = advisor
                        , to = [ me ]
                        , contents = [ "Naolin,",
                          """Don't waste your time. I found a
                          counterexample to the cut elimination theorem
                          (attached).
                          The issue is with your side condition in the
                          ELT-E rule. Remind me why you needed that side
                          condition again?""",
                          "Zhou"
                        ]
                        }
                    , actions =
                        [ Types.Respond
                            { shortText = "Respond"
                            , email =
                                { from = me
                                , to = [ advisor ]
                                , contents = [ "Second response" ]
                                }
                            , next = "b-3"
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
                            , next = "b-4"
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
      , first =
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
            [ { shortText = "Also looking forward"
              , email =
                    { from = me
                    , to = [ college_friend ]
                    , contents = [ """I'm looking forward to it too boo.
                    Haha, don't you know never to ask a grad student when
                    they're finishing their thesis? But honestly I could
                    bail if a good job comes along. Tell me more?""" ]
                    }
              , next = "c-2"
              }
            ]
      , scenes =
            Dict.fromList
                [ ( "c-2"
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
                            , next = "c-3"
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
                            , next = "c-4"
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
    ]
