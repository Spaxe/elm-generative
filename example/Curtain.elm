module Example.Curtain exposing (..)

import Html exposing (Html, div, text)
import Draw exposing (..)
import List.Extra
import Generative exposing (..)
import Random


type Model
    = Empty
    | Curtain (List (List Float)) (List (List Float))


init : ( Model, Cmd Msg )
init =
    update Generate <| Empty


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
        Curtain verticalDrape horizontalShift ->
            a4Landscape
                []
                (initialiseLines 100
                    |> List.map2 (map2First (+)) (accumulateList horizontalShift)
                    |> List.map2 (map2Second (+)) (accumulateList verticalDrape)
                    |> List.map (translateList 40 50)
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
                    (Random.list 100 <|
                        random1D 200 0.6
                    )
                    (Random.list 100 <|
                        random1D 200 0.4
                    )
            )

        Draw data ->
            ( data, Cmd.none )
