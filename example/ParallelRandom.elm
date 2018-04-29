module Example.ParallelRandom exposing (..)

{-| Random rows of squiggle
-}

import Html exposing (Html, div, text)
import Draw exposing (..)
import List.Extra
import Generative exposing (..)
import Random


-- Configuration for use, typically number of repetitions


numberOfLines : Int
numberOfLines =
    10


numberOfSegments : Int
numberOfSegments =
    100


gap : Int
gap =
    10



-- Model contains the generative/random element


type alias Model =
    List (List Float)



-- Msg setup the drawing process


type Msg
    = Generate
    | Draw Model



-- Init just calls the first update


init : ( Model, Cmd Msg )
init =
    update Generate []



-- Setup is called by view, used to create the base SVGs


setup : Int -> List (List ( Float, Float ))
setup n =
    let
        line y =
            makePath numberOfSegments 0 y 110 y
    in
        List.map line <|
            List.Extra.initialize n (toFloat << (*) gap)



-- Composes the generative functions together and draw on screen


view : Model -> Html Msg
view model =
    let
        data =
            setup numberOfLines

        randomValues =
            model

        transformed =
            List.map2 (map2Second (+)) randomValues data
    in
        a4Landscape
            []
            (paths <| List.map (translateList 90 50) transformed)



-- Update is where randomness happens


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( model
            , Random.generate Draw <|
                Random.list numberOfLines <|
                    random1D numberOfSegments (toFloat gap)
            )

        Draw data ->
            ( data, Cmd.none )
