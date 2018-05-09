module Example.Landscape exposing (..)

import Draw exposing (..)
import Generative exposing (..)
import Html exposing (Html, div, text)
import Random


type Model
    = Configuration Int Int
    | Model
        { data : Landscape
        , n : Int
        , segments : Int
        }


type Landscape
    = Landscape ( Float, Float ) Float (List (List Float))


init : ( Model, Cmd Msg )
init =
    update Generate (Configuration 10 1000)


initialiseLines : Int -> Int -> List (List ( Float, Float ))
initialiseLines n segments =
    List.map (\y -> makePath segments 10 y 210 y) <|
        List.repeat n 0


type Msg
    = Generate
    | Draw Landscape


view : Model -> Html Msg
view model =
    case model of
        Model { data, n, segments } ->
            case data of
                Landscape dSunPosition dSunSize crawl ->
                    let
                        lines =
                            initialiseLines n segments

                        randomValues =
                            List.map accumulate crawl

                        shepherdedValues =
                            accumulateList randomValues

                        transformed =
                            List.map2 (map2Second (+)) shepherdedValues lines

                        sunPosition =
                            mapTuple2 ((*) 100) ((*) 50) dSunPosition

                        sunSize =
                            10 + dSunSize * 10
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
    case ( msg, model ) of
        ( Generate, Configuration n segments ) ->
            ( model
            , Random.generate Draw <|
                Random.map3 Landscape
                    randomTuple
                    random
                    (randomList2 n segments)
            )

        ( Draw landscape, Configuration n segments ) ->
            ( Model { data = landscape, n = n, segments = segments }, Cmd.none )

        _ ->
            ( model, Cmd.none )
