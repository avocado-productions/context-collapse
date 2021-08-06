module Message exposing (Element(..), Message)

import Markup exposing (Markup)
import Storage exposing (Storage)


type alias Message =
    { props : Storage
    , contents : List Element
    }


type Element
    = Paragraph Markup
    | Image { url : String }
    | Quote (List Element)
