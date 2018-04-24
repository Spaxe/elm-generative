module Example.Curtain exposing (..)

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
            makePath 100 10 y 210 y
    in
        List.map line <| List.Extra.initialize n (toFloat >> (*) 1.0)


type Msg
    = Generate
    | Draw Data


view : Model -> Html Msg
view model =
    let
        data =
            initialiseLines 100

        randomValues =
            model

        shepherdedValues =
            accumulateList randomValues

        transformed =
            List.map2 (map2Second (+)) shepherdedValues data
    in
        a4Landscape
            [ withStroke 0.4
            ]
            [ g [ translate 40 50 ] (paths transformed)
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( model
            , Random.generate Draw <|
                Random.list 100 <|
                    random1D 200 0.5
            )

        Draw data ->
            ( data, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
