module Attr exposing (..)

import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


error : List (Attribute msg)
error =
    [ Font.color red ]


greenButton : List (Attribute msg)
greenButton =
    [ Background.color green
    , Border.color darkGreen
    , Border.rounded 3

    --    Creates the effect of being extruded
    , Border.widthEach { bottom = 3, top = 0, right = 0, left = 0 }
    , Font.bold
    , Font.color white
    , paddingXY 20 6
    ]


greyButton : List (Attribute ms)
greyButton =
    [ Background.color lightGrey
    , Border.color grey
    , Border.rounded 3
    , Border.widthEach { bottom = 1, right = 1, top = 0, left = 0 }
    , Font.bold
    , Font.color darkCharcoal
    ]


input : List (Attribute msg)
input =
    [ Border.width 1, Border.rounded 3, Border.color lightCharcoal, padding 3 ]
