module Example.Curtain exposing (..)

import Draw exposing (..)
import Generative exposing (..)
import Html exposing (Html, div, text)
import List.Extra
import Random
import Svg.Attributes exposing (transform)


type Model
    = Setup Int
    | Model Int Curtain


type Curtain
    = Curtain (List (List Float)) (List (List Float))


init : ( Model, Cmd Msg )
init =
    update Generate (Setup 100)


initialiseLines : Int -> List (List ( Float, Float ))
initialiseLines n =
    List.map (\y -> makePath 100 10 y 210 y) <|
        List.Extra.initialize n toFloat


type Msg
    = Generate
    | Draw Curtain


view : Model -> Html Msg
view model =
    case model of
        Model n (Curtain a b) ->
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
                [ g
                    [ transform <| Draw.translate 50 30 ]
                    (initialiseLines 100
                        |> List.map2 (map2First (+)) (mapList ((*) 0.25) a)
                        |> List.map2 (map2First (+)) dxs
                        |> List.map2 (map2Second (+)) (mapList ((*) 0.25) b)
                        |> List.map2 (map2Second (+)) dys
                        |> List.map Draw.lines
                    )
                ]

        _ ->
            text ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Generate, Setup n ) ->
            ( model
            , Random.generate Draw <|
                Random.map2 Curtain
                    (randomList2 n 100)
                    (randomList2 n 100)
            )

        ( Draw data, Setup n ) ->
            ( Model n data, Cmd.none )

        _ ->
            ( model, Cmd.none )
