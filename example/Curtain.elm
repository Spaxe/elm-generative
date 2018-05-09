module Example.Curtain exposing (..)

import Draw exposing (..)
import Generative exposing (..)
import Html exposing (Html, div, text)
import List.Extra
import Random


type Model
    = Empty
    | Curtain (List (List Float)) (List (List Float))


init : ( Model, Cmd Msg )
init =
    update Generate Empty


initialiseLines : Int -> List (List ( Float, Float ))
initialiseLines n =
    List.map (\y -> makePath 100 10 y 210 y) <|
        List.Extra.initialize n (toFloat >> (*) 1.0)


type Msg
    = Generate
    | Draw Model


view : Model -> Html Msg
view model =
    case model of
        Curtain a b ->
            let
                dxs =
                    mapList ((*) 0.5) a

                dys =
                    mapList ((*) 0.5) b
            in
            a4Landscape
                []
                (initialiseLines 100
                    |> List.map2 (map2First (+)) (accumulateList dxs)
                    |> List.map2 (map2Second (+)) (accumulateList dys)
                    |> List.map (translateList 40 30)
                    |> paths
                )

        Empty ->
            text ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( model
            , Random.generate Draw <|
                Random.map2 Curtain
                    (randomList2 100 200)
                    (randomList2 100 200)
            )

        Draw data ->
            ( data, Cmd.none )
