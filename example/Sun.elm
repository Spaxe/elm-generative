module Example.Sun exposing (..)

import Draw exposing (..)
import Generative exposing (..)
import Html exposing (Html, div, text)
import Random
import Svg.Attributes exposing (transform)


type Model
    = Setup Configuration
    | Model Configuration Sun


type Configuration
    = Configuration Int Int Float Float


type Sun
    = Sun (List (List ( Float, Float )))


init : ( Model, Cmd Msg )
init =
    update Generate (Setup <| Configuration 128 128 25 60)


initialiseLines : Configuration -> List (List ( Float, Float ))
initialiseLines (Configuration n segments r1 r2) =
    let
        line ( x1, y1 ) ( x2, y2 ) =
            makePath segments x1 y1 x2 y2
    in
    List.range 1 n
        |> List.map
            (\x -> toFloat x / 48.0 * 360)
        |> List.map
            (\x -> line (fromPolar ( r1, x )) (fromPolar ( r2, x )))


type Msg
    = Generate
    | Draw Sun


view : Model -> Html Msg
view model =
    case model of
        Model config (Sun crawl) ->
            let
                data =
                    initialiseLines config

                amplitude a =
                    mapList <| mapTuple <| (*) a

                shepherdLines x =
                    List.map accumulateTuple x

                shepherdedValues =
                    accumulateListTuple <| shepherdLines <| amplitude 0.5 crawl

                transformed =
                    List.map2 (map2Tuple (+)) shepherdedValues data
            in
            a4Landscape
                []
                [ g
                    [ transform <| Draw.translate 148 105 ]
                    (List.map Draw.lines transformed)
                ]

        _ ->
            text ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Generate, Setup (Configuration n segments _ _) ) ->
            ( model
            , Random.generate Draw <|
                Random.map Sun
                    (randomListTuple2 n segments)
            )

        ( Draw data, Setup config ) ->
            ( Model config data, Cmd.none )

        _ ->
            ( model, Cmd.none )
