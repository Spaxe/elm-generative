module Example.Curtain exposing (..)

import Draw exposing (..)
import Generative exposing (..)
import Html exposing (Html, div, text)
import List.Extra
import Random


type Model
    = Configuration Int
    | Model
        { data : Curtain
        , n : Int
        }


type Curtain
    = Curtain (List (List Float)) (List (List Float))


init : ( Model, Cmd Msg )
init =
    update Generate (Configuration 100)


initialiseLines : Int -> List (List ( Float, Float ))
initialiseLines n =
    List.map (\y -> makePath 100 10 y 210 y) <|
        List.Extra.initialize n (toFloat >> (*) 1.0)


type Msg
    = Generate
    | Draw Curtain


view : Model -> Html Msg
view model =
    case model of
        Model { data, n } ->
            case data of
                Curtain a b ->
                    let
                        dxs =
                            mapList ((*) 0.5) a
                                |> accumulateList

                        dys =
                            mapList ((*) 0.5) b
                                |> accumulateList
                    in
                    a4Landscape
                        []
                        (initialiseLines 100
                            |> List.map2 (map2First (+)) dxs
                            |> List.map2 (map2Second (+)) dys
                            |> List.map (translateList 40 30)
                            |> paths
                        )

        _ ->
            text ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Generate, Configuration n ) ->
            ( model
            , Random.generate Draw <|
                Random.map2 Curtain
                    (randomList2 n 200)
                    (randomList2 n 200)
            )

        ( Draw curtain, Configuration n ) ->
            ( Model { data = curtain, n = n }, Cmd.none )

        _ ->
            ( model, Cmd.none )
