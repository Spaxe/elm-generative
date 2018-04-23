module Crawl exposing (..)

import Html exposing (Html, div, text)
import Draw exposing (..)
import List.Extra
import Generative exposing (..)
import Random


type alias Data =
    List (List Float)


type alias Model =
    Data


init : ( Model, Cmd Msg )
init =
    update Generate []


initialiseLines : Int -> List (List ( Float, Float ))
initialiseLines n =
    let
        line y =
            makePath 100 10 y 110 y
    in
        List.map line <| List.Extra.initialize n (toFloat >> (*) 10.0)


type Msg
    = Generate
    | Draw Data


view : Model -> Html Msg
view model =
    let
        data =
            initialiseLines 10

        generative =
            model

        transformed =
            List.map2 (mapY (+)) generative data
    in
        a4Landscape
            [ withStroke 0.4
            ]
            [ g [ translate 10 10 ] (simplePaths transformed)
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( model
            , Random.generate Draw <|
                Random.list 18 <|
                    random1D 100 1
            )

        Draw data ->
            ( data, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
