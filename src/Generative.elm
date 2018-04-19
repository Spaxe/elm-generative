module Generative exposing (frame)

{-| Generative art is a lot of fun. This package aims to help you tinker.


# Setting it up

@docs frame

-}

import Html exposing (Html, text, main_)
import Svg.Attributes exposing (class, width, height, viewBox, x, y, rx, ry)
import Svg exposing (svg, rect)


type alias Width =
    Int


type alias Height =
    Int


{-| Create a SVG container with the size of our choice.

Width and Height are `Int`s.

-}
frame : Width -> Height -> Html msg
frame w h =
    svg
        [ width <| toString w
        , height <| toString h
        , viewBox <| "0 0 " ++ (toString w) ++ " " ++ (toString h)
        ]
        [ rect
            [ x "10"
            , y "10"
            , width "100"
            , height "100"
            , rx "15"
            , ry "15"
            ]
            []
        ]
