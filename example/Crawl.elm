module Crawl exposing (..)

import Html exposing (Html, div, text)
import Draw exposing (..)
import Generative exposing (..)
import Random


type alias Data =
    List (List Float)


type alias Model =
    Data


init : ( Model, Cmd Msg )
init =
    update Generate []


type Msg
    = Generate
    | Draw Data


view : Model -> Html Msg
view model =
    a4Landscape
        [ withStroke 0.4
        ]
        [ g [ translate 10 10 ] (simplePaths model)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( model
            , Random.generate Draw <|
                Random.list 18 <|
                    random1D 28 1
            )

        Draw data ->
            ( data, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
