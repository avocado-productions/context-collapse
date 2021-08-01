module Markup exposing (Markup, Style, Text(..), plain)


type Text
    = Raw Style String
    | Link { url : String, contents : Markup }


type alias Markup =
    List Text


type alias Style =
    { italic : Bool, bold : Bool, strike : Bool }


plain : Style
plain =
    { italic = False, bold = False, strike = False }
