module Example.Landscape exposing (..)

import Draw exposing (..)
import Generative exposing (..)
import Html exposing (Html, div, text)
import Random


type Model
    = Empty
    | Model
        { data : Landscape
        , n : Int
        , segments : Int
        }


type Landscape
    = Landscape ( Float, Float ) Float (List (List Float))


init : ( Model, Cmd Msg )
init =
    update (Generate 10 1000) Empty


initialiseLines : Int -> Int -> List (List ( Float, Float ))
initialiseLines n segments =
    List.map (\y -> makePath segments 10 y 210 y) <|
        List.repeat n 0


type Msg
    = Generate Int Int
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

        Empty ->
            text ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Generate n segments, Model m ) ->
            ( Model { m | n = n, segments = segments }
            , Random.generate Draw <|
                Random.map3 Landscape
                    randomTuple
                    random
                    (randomList2 n segments)
            )

        ( Draw landscape, Model m ) ->
            ( Model { m | data = landscape }, Cmd.none )

        ( _, Empty ) ->
            ( Empty, Cmd.none )
