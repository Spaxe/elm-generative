module Draw exposing
    ( a4Landscape
    , a4Portrait
    , circle
    , clipPath
    , g
    , line
    , lines
    , rect
    , rotate
    , scale
    , strokeWidth
    , translate
    , urlHash
    )

{-| SVG Drawing helper functions
-}

import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg, g, rect, svg)
import Svg.Attributes as Attributes


{-| A4 sized container in landscape
-}
a4Landscape : List (Svg.Attribute msg) -> List (Svg msg) -> Html msg
a4Landscape =
    frame 297 210 "mm"


{-| A4 sized container in portrait
-}
a4Portrait : List (Svg.Attribute msg) -> List (Svg msg) -> Html msg
a4Portrait =
    frame 210 297 "mm"


{-| Group of SVG elements, useful for styling and positioning.
-}
g : List (Svg.Attribute msg) -> List (Svg msg) -> Svg msg
g =
    Svg.g


{-| Constructs the transform property for translate
-}
translate : Float -> Float -> String
translate x y =
    " translate(" ++ fromFloat x ++ "," ++ fromFloat y ++ ") "


{-| Constructs the transform property for rotate
-}
rotate : Float -> String
rotate degree =
    " rotate(" ++ fromFloat degree ++ ") "


{-| Constructs the transform property for scale
-}
scale : Float -> String
scale ratio =
    " scale(" ++ fromFloat ratio ++ ") "


{-| Constructs the style property for stroke-width
-}
strokeWidth : Float -> String
strokeWidth x =
    "stroke-width: " ++ fromFloat x ++ "px"


{-| Draw a rectangle
-}
rect : Float -> Float -> Float -> Float -> Svg msg
rect x y width height =
    Svg.rect
        [ Attributes.x <| fromFloat x
        , Attributes.y <| fromFloat y
        , Attributes.width <| fromFloat width
        , Attributes.height <| fromFloat height
        ]
        []


{-| Draw a circle
-}
circle : Float -> Float -> Float -> Svg msg
circle x y r =
    Svg.circle
        [ Attributes.cx <| fromFloat x
        , Attributes.cy <| fromFloat y
        , Attributes.r <| fromFloat r
        ]
        []


{-| Draw a line
-}
line : Float -> Float -> Float -> Float -> Svg msg
line x1 y1 x2 y2 =
    Svg.line
        [ Attributes.x1 <| fromFloat x1
        , Attributes.y1 <| fromFloat y1
        , Attributes.x2 <| fromFloat x2
        , Attributes.y2 <| fromFloat y2
        ]
        []


{-| Draw lines from a list of points
-}
lines : List ( Float, Float ) -> Svg msg
lines points =
    let
        coords ( x, y ) =
            fromFloat x ++ ", " ++ fromFloat y
    in
    Svg.path
        [ List.map coords points
            |> String.join " L "
            |> (++) "M "
            |> Attributes.d
        ]
        []


{-| Define a clipping path with name.

**Note**: This is ignored by the AxiDraw.

-}
clipPath : String -> List (Svg msg) -> Svg msg
clipPath name element =
    Svg.defs
        []
        [ Svg.clipPath
            [ Attributes.id name ]
            element
        ]


urlHash : String -> String
urlHash name =
    "url(#" ++ name ++ ")"


{-| A fixed sized SVG container.
-}
frame :
    Float
    -> Float
    -> String
    -> List (Svg.Attribute msg)
    -> List (Svg msg)
    -> Html msg
frame width height unit attributes =
    svg <|
        [ Attributes.style <|
            "width: "
                ++ fromFloat width
                ++ unit
                ++ "; height: "
                ++ fromFloat height
                ++ unit
        , Attributes.viewBox <|
            "0 0 "
                ++ fromFloat width
                ++ " "
                ++ fromFloat height
        , Attributes.fill "none"
        , Attributes.stroke "black"
        , Attributes.strokeLinecap "round"
        , Attributes.strokeLinejoin "round"
        , Attributes.id "frame"
        , Attributes.strokeWidth "0.4"
        , Attributes.width <| fromFloat width ++ unit
        , Attributes.height <| fromFloat height ++ unit
        , Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg"
        ]
            ++ attributes
