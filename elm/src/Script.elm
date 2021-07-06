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


a : Types.AddressbookEntry
a =
    Contact.create
        |> Contact.email "a@letter.com"
        |> Contact.short "A"
        |> Contact.full "A. AAAAAAA"


b : Types.AddressbookEntry
b =
    Contact.create
        |> Contact.email "b@letter.com"
        |> Contact.short "B"
        |> Contact.full "B. BBBBBB"


c : Types.AddressbookEntry
c =
    Contact.create
        |> Contact.email "c@letter.com"
        |> Contact.short "C"
        |> Contact.full "C. CCCCCCCC"


myScript : List Types.ThreadScript
myScript =
    [ { id = "convo-a"
      , subject = "Conversation with A"
      , first =
            { from = a
            , to = [ me ]
            , contents = [ "Hello world" ]
            }
      , actions =
            [ { shortText = "Response"
              , email =
                    { from = me
                    , to = [ a ]
                    , contents = [ "Response" ]
                    }
              , next = "a-one"
              }
            , { shortText = "Duplicate"
              , email =
                    { from = me
                    , to = [ a ]
                    , contents = [ "Duplicate" ]
                    }
              , next = "a-one"
              }
            ]
      , scenes =
            Dict.fromList
                [ ( "a-one"
                  , { receivedEmail =
                        { from = a
                        , to = [ me ]
                        , contents = [ "Hello world" ]
                        }
                    , actions =
                        [ Types.Respond
                            { shortText = "Go Left"
                            , email =
                                { from = me
                                , to = [ a ]
                                , contents = [ "Go Left" ]
                                }
                            , next = "a-two"
                            }
                        , Types.Respond
                            { shortText = "Go Right"
                            , email =
                                { from = me
                                , to = [ a ]
                                , contents = [ "Go Right" ]
                                }
                            , next = "a-three"
                            }
                        ]
                    }
                  )
                , ( "a-two"
                  , { receivedEmail =
                        { from = a
                        , to = [ me ]
                        , contents = [ "Good ending" ]
                        }
                    , actions =
                        []
                    }
                  )
                , ( "a-three"
                  , { receivedEmail =
                        { from = a
                        , to = [ me ]
                        , contents = [ "Bad ending starts" ]
                        }
                    , actions =
                        [ Types.Immediate "a-four" ]
                    }
                  )
                , ( "a-four"
                  , { receivedEmail =
                        { from = a
                        , to = [ me ]
                        , contents = [ "Bad ending continues" ]
                        }
                    , actions =
                        [ Types.Immediate "a-five" ]
                    }
                  )
                , ( "a-five"
                  , { receivedEmail =
                        { from = a
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
      , subject = "Conversation with B"
      , first =
            { from = b
            , to = [ me ]
            , contents = [ "First email" ]
            }
      , actions =
            [ { shortText = "Respond"
              , email =
                    { from = me
                    , to = [ b ]
                    , contents = [ "First response" ]
                    }
              , next = "b-2"
              }
            ]
      , scenes =
            Dict.fromList
                [ ( "b-2"
                  , { receivedEmail =
                        { from = b
                        , to = [ me ]
                        , contents = [ "Second email" ]
                        }
                    , actions =
                        [ Types.Respond
                            { shortText = "Respond"
                            , email =
                                { from = me
                                , to = [ b ]
                                , contents = [ "Second response" ]
                                }
                            , next = "b-3"
                            }
                        ]
                    }
                  )
                , ( "b-3"
                  , { receivedEmail =
                        { from = b
                        , to = [ me ]
                        , contents = [ "Third email" ]
                        }
                    , actions =
                        [ Types.Respond
                            { shortText = "Respond"
                            , email =
                                { from = me
                                , to = [ b ]
                                , contents = [ "Third response" ]
                                }
                            , next = "b-4"
                            }
                        ]
                    }
                  )
                , ( "b-4"
                  , { receivedEmail =
                        { from = b
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
      , subject = "Conversation with C"
      , first =
            { from = c
            , to = [ me ]
            , contents = [ "First email" ]
            }
      , actions =
            [ { shortText = "Respond"
              , email =
                    { from = me
                    , to = [ c ]
                    , contents = [ "First response" ]
                    }
              , next = "c-2"
              }
            ]
      , scenes =
            Dict.fromList
                [ ( "c-2"
                  , { receivedEmail =
                        { from = c
                        , to = [ me ]
                        , contents = [ "Second email" ]
                        }
                    , actions =
                        [ Types.Respond
                            { shortText = "Respond"
                            , email =
                                { from = me
                                , to = [ c ]
                                , contents = [ "Second response" ]
                                }
                            , next = "c-3"
                            }
                        ]
                    }
                  )
                , ( "c-3"
                  , { receivedEmail =
                        { from = c
                        , to = [ me ]
                        , contents = [ "Third email" ]
                        }
                    , actions =
                        [ Types.Respond
                            { shortText = "Respond"
                            , email =
                                { from = me
                                , to = [ c ]
                                , contents = [ "Third response" ]
                                }
                            , next = "c-4"
                            }
                        ]
                    }
                  )
                , ( "c-4"
                  , { receivedEmail =
                        { from = c
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
