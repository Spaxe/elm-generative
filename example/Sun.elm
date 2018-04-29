module Example.Sun exposing (..)

import Html exposing (Html, div, text)
import Draw exposing (..)
import Generative exposing (..)
import Random


numberOfLines : Int
numberOfLines =
    10


numberOfSegments : Int
numberOfSegments =
    1000


type Model
    = Empty
    | Sun ( Float, Float ) Float (List (List Float))


init : ( Model, Cmd Msg )
init =
    update Generate <| Empty


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
    case model of
        Sun dSunPosition dSunSize crawl ->
            let
                data =
                    initialiseLines numberOfLines

                randomValues =
                    List.map accumulate crawl

                shepherdedValues =
                    accumulateList randomValues

                transformed =
                    List.map2 (map2Second (+)) shepherdedValues data

                sunPosition =
                    dSunPosition

                sunSize =
                    10 + dSunSize
            in
                a4Landscape
                    []
                    [ g [] (paths <| List.map (translateList 40 100) transformed)
                    , g [] [ uncurry circle (translate 150 40 sunPosition) sunSize [] ]
                    ]

        _ ->
            text ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( model
            , Random.generate Draw <|
                Random.map3 Sun
                    (Random.pair (random 100) (random 50))
                    (random 10)
                    (Random.list numberOfLines <|
                        random1D numberOfSegments 1
                    )
            )

        Draw data ->
            ( data, Cmd.none )
