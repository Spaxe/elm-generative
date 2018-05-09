module Example.Landscape exposing (..)

import Draw exposing (..)
import Generative exposing (..)
import Html exposing (Html, div, text)
import Random
import Tuple exposing (mapFirst, mapSecond)


numberOfLines : Int
numberOfLines =
    10


numberOfSegments : Int
numberOfSegments =
    1000


type Model
    = Landscape ( Float, Float ) Float (List (List Float))


init : ( Model, Cmd Msg )
init =
    update Generate <| Landscape ( 0, 0 ) 0 []


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
        Landscape dSunPosition dSunSize crawl ->
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
                        |> mapFirst ((*) 100)
                        |> mapSecond ((*) 50)

                sunSize =
                    10 + dSunSize * 10
            in
            a4Landscape
                []
                [ g [] (paths <| List.map (translateList 40 100) transformed)
                , g [] [ uncurry circle (translate 150 40 sunPosition) sunSize [] ]
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( model
            , Random.generate Draw <|
                Random.map3 Landscape
                    randomTuple
                    random
                    (randomList2 numberOfLines numberOfSegments)
            )

        Draw data ->
            ( data, Cmd.none )
