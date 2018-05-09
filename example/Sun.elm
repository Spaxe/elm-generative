module Example.Sun exposing (..)

import Draw exposing (..)
import Generative exposing (..)
import Html exposing (Html, div, text)
import Random


numberOfLines : Int
numberOfLines =
    128


numberOfSegments : Int
numberOfSegments =
    128


radialSmallRadius : Float
radialSmallRadius =
    25


radialLargeRadius : Float
radialLargeRadius =
    60


type Model
    = Empty
    | Sun Float (List (List ( Float, Float )))


init : ( Model, Cmd Msg )
init =
    update Generate <| Empty


initialiseLines : Int -> List (List ( Float, Float ))
initialiseLines n =
    let
        line ( x1, y1 ) ( x2, y2 ) =
            makePath numberOfSegments x1 y1 x2 y2
    in
    List.range 1 n
        |> List.map
            (\x -> toFloat x / 48.0 * 360)
        |> List.map
            (\x ->
                line
                    (fromPolar
                        ( radialSmallRadius, x )
                    )
                    (fromPolar
                        ( radialLargeRadius, x )
                    )
            )


type Msg
    = Generate
    | Draw Model


view : Model -> Html Msg
view model =
    case model of
        Sun dSunSize crawl ->
            let
                data =
                    initialiseLines numberOfLines

                randomValues =
                    List.map accumulateTuple crawl

                shepherdedValues =
                    accumulateTupleList randomValues

                transformed =
                    List.map2 (map2Tuple (+)) shepherdedValues data

                sunSize =
                    20 + dSunSize * 20
            in
            a4Landscape
                []
                [ g [] (paths <| List.map (translateList 148 105) transformed)
                ]

        _ ->
            text ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( model
            , Random.generate Draw <|
                Random.map2 Sun
                    random
                    (Random.list numberOfLines <|
                        Random.list numberOfSegments <|
                            randomTuple
                    )
            )

        Draw data ->
            ( data, Cmd.none )
