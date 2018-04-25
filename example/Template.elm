module Example.Template exposing (..)

import Html exposing (Html, div, text)
import Draw exposing (..)
import List.Extra
import Generative exposing (..)
import Random


numberOfLines : Int
numberOfLines =
    10


numberOfSegments : Int
numberOfSegments =
    100


gap : Int
gap =
    10


type alias Model =
    List (List Float)


type Msg
    = Generate
    | Draw Model


init : ( Model, Cmd Msg )
init =
    update Generate []


setup : Int -> List (List ( Float, Float ))
setup n =
    let
        line y =
            makePath numberOfSegments 0 y 110 y
    in
        List.map line <|
            List.Extra.initialize n (toFloat << (*) gap)


view : Model -> Html Msg
view model =
    let
        data =
            setup numberOfLines

        randomValues =
            model

        transformed =
            List.map2 (map2Second (+)) randomValues data
    in
        a4Landscape
            [ withStroke 0.4
            ]
            [ g [ translate 90 50 ] (paths transformed)
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( model
            , Random.generate Draw <|
                Random.list numberOfLines <|
                    random1D numberOfSegments (toFloat gap)
            )

        Draw data ->
            ( data, Cmd.none )
