module Draw
    exposing
        ( a4Landscape
        , a4Portrait
        , rect
        , g
        , circle
        , line
        , path
        , paths
        )

{-| SVG Drawing helper functions
-}

import Html exposing (Html)
import Html.Attributes
import Svg.Attributes as Attributes
import Svg exposing (Svg, svg, g, rect)


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


{-| Draw a rectangle
-}
rect : Float -> Float -> Float -> Float -> List (Svg msg) -> Svg msg
rect x y width height =
    Svg.rect
        [ Attributes.x <| toString x
        , Attributes.y <| toString y
        , Attributes.width <| toString width
        , Attributes.height <| toString height
        ]


{-| Draw a circle
-}
circle : Float -> Float -> Float -> List (Svg msg) -> Svg msg
circle x y r =
    Svg.circle
        [ Attributes.cx <| toString x
        , Attributes.cy <| toString y
        , Attributes.r <| toString r
        ]


{-| Draw a line
-}
line : Float -> Float -> Float -> Float -> List (Svg msg) -> Svg msg
line x1 y1 x2 y2 =
    Svg.line
        [ Attributes.x1 <| toString x1
        , Attributes.y1 <| toString y1
        , Attributes.x2 <| toString x2
        , Attributes.y2 <| toString y2
        ]


{-| Draw lines from a list of points
-}
path : List ( Float, Float ) -> Svg msg
path points =
    let
        coords ( x, y ) =
            (toString x) ++ ", " ++ (toString y)
    in
        Svg.path
            [ List.map coords points
                |> String.join " L "
                |> (++) "M "
                |> Attributes.d
            ]
            []


{-| Draw a list of paths
-}
paths : List (List ( Float, Float )) -> List (Svg msg)
paths pointsList =
    List.map path pointsList


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
                ++ (toString width)
                ++ unit
                ++ "; height: "
                ++ (toString height)
                ++ unit
        , Attributes.viewBox <|
            "0 0 "
                ++ (toString width)
                ++ " "
                ++ (toString height)
        , Attributes.fill "none"
        , Attributes.stroke "black"
        , Attributes.id "frame"
        , Attributes.strokeWidth "0.4"
        , Attributes.width <| (toString width) ++ unit
        , Attributes.height <| (toString height) ++ unit
        , Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg"
        ]
            ++ attributes
