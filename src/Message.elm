module Message exposing (Element(..), Message)

import Markup exposing (Markup)
import Props exposing (Props)


type alias Message =
    { props : Props
    , contents : List Element
    }


type Element
    = Paragraph Markup
    | Image { url : String }
    | Quote (List Element)
