module Message exposing (Element(..), Message)

import Markup exposing (Markup)


type alias Message =
    { props : List { key : String, value : String }
    , contents : List Element
    }


type Element
    = Paragraph Markup
    | Image { url : String }
    | Quote (List Element)
