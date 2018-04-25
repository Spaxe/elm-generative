module Example.Landscape exposing (..)

import Html exposing (Html, div, text)
import Draw exposing (..)
import Generative exposing (..)
import Random


numberOfLines : Int
numberOfLines =
    20


numberOfSegments : Int
numberOfSegments =
    1000


type alias Model =
    List (List Float)


init : ( Model, Cmd Msg )
init =
    update Generate []


initialiseLines : Int -> List (List ( Float, Float ))
initialiseLines n =
    let
        line y =
            makePath numberOfSegments 10 y 210 y
    in
        List.map line <|
            List.repeat n 0


type Msg
    = Generate
    | Draw Model


view : Model -> Html Msg
view model =
    let
        data =
            initialiseLines numberOfLines

        randomValues =
            List.map accumulate model

        shepherdedValues =
            accumulateList randomValues

        transformed =
            List.map2 (map2Second (+)) shepherdedValues data
    in
        a4Landscape
            [ withStroke 0.4
            ]
            [ g [ translate 40 100 ] (paths transformed)
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( model
            , Random.generate Draw <|
                Random.list numberOfLines <|
                    random1D numberOfSegments 1
            )

        Draw data ->
            ( data, Cmd.none )
