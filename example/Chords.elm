module Example.Chords exposing (..)

import Draw exposing (..)
import Generative exposing (..)
import Html exposing (Html, div, text)
import Random
import Svg.Attributes exposing (clipPath, transform)


type Model
    = Setup
    | Model Chords


type Chords
    = Chords (List Float) (List Float)


init : ( Model, Cmd Msg )
init =
    update Generate Setup


type Msg
    = Generate
    | Draw Chords


view : Model -> Html Msg
view model =
    case model of
        Model (Chords p q) ->
            let
                s =
                    60.0

                circle =
                    Draw.circle 0 0 s

                x a =
                    s * cos (a * 360)

                y a =
                    s * sin (a * 360)

                lines =
                    List.map2
                        (\a b ->
                            g
                                []
                                [ Draw.line (x a) (y a) (x b) (y b) ]
                        )
                        p
                        q
            in
            a4Landscape
                []
                [ g
                    [ transform <| Draw.translate 150 100
                    ]
                    lines
                ]

        _ ->
            text ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Generate, Setup ) ->
            ( model
            , Random.generate Draw <|
                Random.map2 Chords
                    (randomList 20)
                    (randomList 20)
            )

        ( Draw data, Setup ) ->
            ( Model data, Cmd.none )

        _ ->
            ( model, Cmd.none )
