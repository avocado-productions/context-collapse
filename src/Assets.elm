module Assets exposing (idCircle, importantNo, importantYes, starNo, starYes, attachedDocument)

import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes


idCircle : Svg msg
idCircle =
    Svg.svg [ Html.Attributes.width 50, Html.Attributes.height 50 ]
        [ Svg.circle [ Svg.Attributes.fill "lightgray", Svg.Attributes.cx "25", Svg.Attributes.cy "25", Svg.Attributes.r "25" ] []
        , Svg.circle [ Svg.Attributes.fill "gray", Svg.Attributes.cx "25", Svg.Attributes.cy "19", Svg.Attributes.r "5" ] []
        , Svg.polygon [ Svg.Attributes.fill "gray", Svg.Attributes.points "15,35 22,28 28,28 35,35" ] []
        ]


importantNo : Svg msg
importantNo =
    Svg.svg [ Html.Attributes.width 24, Html.Attributes.height 10 ]
        [ Svg.polygon
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke "gray"
            , Svg.Attributes.points "0,0 13,0 18,5 13,10 0,10 5,5"
            ]
            []
        ]


importantYes : Svg msg
importantYes =
    Svg.svg [ Html.Attributes.width 24, Html.Attributes.height 10 ]
        [ Svg.polygon
            [ Svg.Attributes.fill "yellow"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.points "0,0 13,0 18,5 13,10 0,10 5,5"
            ]
            []
        ]


starNo : Svg msg
starNo =
    Svg.svg [ Html.Attributes.width 20, Html.Attributes.height 19 ]
        [ Svg.polygon
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke "gray"
            , Svg.Attributes.points "4,19 4,12 1,7 7,5 10,0 13,5 19,7 16,12 16,19 10,16.5"
            ]
            []
        ]


starYes : Svg msg
starYes =
    Svg.svg [ Html.Attributes.width 20, Html.Attributes.height 19 ]
        [ Svg.polygon
            [ Svg.Attributes.fill "yellow"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.points "4,19 4,12 1,7 7,5 10,0 13,5 19,7 16,12 16,19 10,16.5"
            ]
            []
        ]


attachedDocument : Svg msg
attachedDocument =
    Svg.svg [ Html.Attributes.width 15, Html.Attributes.height 15 ]
        [ Svg.polygon
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke "gray"
            , Svg.Attributes.points "3,1 8,1 12,5, 8,5 8,1 12,5 12,14 3,14"
            ]
            [] 
        , Svg.line [Svg.Attributes.stroke "gray", Svg.Attributes.points "5"] []
        ]
