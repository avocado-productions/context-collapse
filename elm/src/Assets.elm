module Assets exposing (importantNo, importantYes, starNo)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes

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
