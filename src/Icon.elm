module Icon exposing (..)

-- This module is supposed to replace jasonliang512/elm-heroicons
-- due to his recent username's change.

import Html exposing (Html)
import Html.Attributes exposing (attribute)
import Svg exposing (..)
import Svg.Attributes exposing (..)


download : Html msg
download =
    svg [ fill "none", attribute "stroke" "currentColor", viewBox "0 0 24 24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
        [ Svg.path [ d "M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-4l-4 4m0 0l-4-4m4 4V4", attribute "stroke-linecap" "round", attribute "stroke-linejoin" "round", attribute "stroke-width" "2" ]
            []
        ]


questionMark : Html msg
questionMark =
    svg [ fill "none", attribute "stroke" "currentColor", viewBox "0 0 24 24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
        [ Svg.path [ d "M8.228 9c.549-1.165 2.03-2 3.772-2 2.21 0 4 1.343 4 3 0 1.4-1.278 2.575-3.006 2.907-.542.104-.994.54-.994 1.093m0 3h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z", attribute "stroke-linecap" "round", attribute "stroke-linejoin" "round", attribute "stroke-width" "2" ]
            []
        ]
