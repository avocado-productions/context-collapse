module UI exposing (..)

import Color
import Element exposing (Element, Length, el, fill, height, none, px, width)
import Element.Background as Background
import Element.Font as Font


threadHeight : Length
threadHeight =
    px 35


leftMenuWidth : Int
leftMenuWidth =
    250


logoWidth : Int
logoWidth =
    30


logoHeight : Int
logoHeight =
    42


brandNameWidth : Int
brandNameWidth =
    leftMenuWidth - logoWidth - 2 * externalChromePadding


externalChromePadding : Int
externalChromePadding =
    35


leftBuffer : Length
leftBuffer =
    -- leftBuffer = leftBuffer1 + leftBuffer2
    px 70


leftBuffer1 : Length
leftBuffer1 =
    -- leftBuffer = leftBuffer1 + leftBuffer2
    px 35


leftBuffer2 : Length
leftBuffer2 =
    -- leftBuffer = leftBuffer1 + leftBuffer2
    px 35


buttonSpacing : Int
buttonSpacing =
    20


rightBuffer : Length
rightBuffer =
    px 35


responseSeparator : Length
responseSeparator =
    px 20


separator : Element msg
separator =
    el [ height (px 1), Background.color Color.uiGray, width fill ] none


uiFont : Element.Attribute msg
uiFont =
    Font.family [ Font.typeface "BenchNine", Font.sansSerif ]


logoFont : Element.Attribute msg
logoFont =
    Font.family [ Font.typeface "Yanone Kaffeesatz", Font.sansSerif ]


contentFont : Element.Attribute msg
contentFont =
    Font.family [ Font.typeface "Raleway", Font.sansSerif ]
