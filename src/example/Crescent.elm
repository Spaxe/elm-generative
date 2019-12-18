module Example.Crescent exposing (Configuration(..), Grid(..), Model(..), Msg(..), Shape, draw, init, setup, square, update, view)

import Draw exposing (..)
import Generative exposing (..)
import Html exposing (Html, div, text)
import Random
import Random.Extra exposing (sample)
import Svg exposing (Svg)
import Svg.Attributes exposing (d, transform)
import Svg.PathD exposing (Segment(..), pathD)


type Model
    = Setup Configuration
    | Model Configuration Grid


type Configuration
    = Configuration Int Float


type Grid
    = Grid (List (Maybe Float))


type alias Shape =
    ( ( Float, Float ), ( Float, Float ), ( Float, Float ) )


type Msg
    = Generate
    | Draw Grid


init : ( Model, Cmd Msg )
init =
    update Generate (Setup <| Configuration 31 5)


setup : Shape
setup =
    ( ( -0.5, 0.5 )
    , ( 0.5, 0.5 )
    , ( 0.5, -0.5 )
    )


draw : Model -> Shape -> List (Svg Msg)
draw model ( a, b, c ) =
    case model of
        Model (Configuration m size) (Grid x) ->
            let
                radius =
                    (toFloat << floor) (toFloat m / 2)

                circlePos cx =
                    cx - radius

                distance dx dy =
                    sqrt (square (circlePos dx) + square (circlePos dy))

                -- | Here we want to make a cresent, so we take the difference
                -- | between two circles for the inside test.
                isVisible dx dy =
                    (distance dx dy <= radius)
                        && (distance dx (dy + radius / 1.5) > radius)
            in
            List.map2
                (\r ( dx, dy ) ->
                    if isVisible dx dy then
                        g
                            [ transform <|
                                Draw.scale size
                                    ++ Draw.translate dx dy
                                    ++ Draw.rotate (Maybe.withDefault 0 r)
                            , Svg.Attributes.style <| strokeWidth (0.5 / size)
                            ]
                            [ Svg.path
                                [ d <|
                                    pathD
                                        [ M a
                                        , L b
                                        , L c
                                        , Q b a
                                        ]
                                , Svg.Attributes.fill "black"
                                , Svg.Attributes.stroke "none"
                                ]
                                []
                            ]

                    else
                        text ""
                )
                x
                (makeGrid m m)

        _ ->
            []


view : Model -> Html Msg
view model =
    case model of
        Model config (Grid d) ->
            a4Landscape
                []
                [ g
                    [ transform <| Draw.translate 150 -10 ++ Draw.rotate 45 ]
                    (setup |> draw model)
                ]

        _ ->
            text ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Generate, Setup (Configuration m _) ) ->
            ( model
            , Random.generate Draw <|
                Random.map Grid (Random.list (square m) <| sample [ 0, 90, 180, 270 ])
            )

        ( Draw data, Setup config ) ->
            ( Model config data, Cmd.none )

        _ ->
            ( model, Cmd.none )


square : number -> number
square x =
    x * x
