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
    let
        line y =
            makePath 100 10 y 210 y
    in
        List.map line <| List.Extra.initialize n (toFloat >> (*) 1.0)


type Msg
    = Generate
    | Draw Model


view : Model -> Html Msg
view model =
    case model of
        Curtain verticalDrape horizontalShift ->
            let
                data =
                    initialiseLines 100

                drapeAmount =
                    accumulateList verticalDrape

                shiftAmount =
                    accumulateList horizontalShift

                drape =
                    List.map2 (map2Second (+)) drapeAmount

                shift =
                    List.map2 (map2First (+)) shiftAmount

                transformed =
                    (drape >> shift) data
            in
                a4Landscape
                    [ withStroke 0.4
                    ]
                    [ g [ translate 40 50 ] (paths transformed)
                    ]

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
