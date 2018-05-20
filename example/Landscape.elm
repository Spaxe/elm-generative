module Example.Landscape exposing (..)

import Draw exposing (..)
import Generative exposing (..)
import Html exposing (Html, div, text)
import Random
import Svg.Attributes exposing (transform)


type Model
    = Setup Int Int
    | Model Int Int Landscape


type Landscape
    = Landscape ( Float, Float ) Float (List (List Float))


init : ( Model, Cmd Msg )
init =
    update Generate (Setup 10 1000)


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
        Model n segments (Landscape dSunPosition dSunSize crawl) ->
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
                [ g [ transform <| Draw.translate 40 100 ] (List.map Draw.lines transformed)
                , g [ transform <| Draw.translate 150 40 ] [ uncurry circle sunPosition sunSize ]
                ]

        _ ->
            text ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Generate, Setup n segments ) ->
            ( model
            , Random.generate Draw <|
                Random.map3 Landscape
                    randomTuple
                    random
                    (randomList2 n segments)
            )

        ( Draw data, Setup n segments ) ->
            ( Model n segments data, Cmd.none )

        _ ->
            ( model, Cmd.none )
