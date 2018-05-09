module Example.ParallelRandom exposing (..)

{-| Random rows of squiggle
-}

import Draw exposing (..)
import Generative exposing (..)
import Html exposing (Html, div, text)
import List.Extra
import Random


-- Model contains the generative/random element


type Model
    = Setup Configuration
    | Model Configuration ParallelLines


type Configuration
    = Configuration Int Int Int


type alias ParallelLines =
    List (List Float)



-- Msg setup the drawing process


type Msg
    = Generate
    | Draw ParallelLines



-- Init just calls the first update


init : ( Model, Cmd Msg )
init =
    update Generate (Setup <| Configuration 10 100 10)



-- Setup is called by view, used to create the base SVGs


setup : Configuration -> List (List ( Float, Float ))
setup (Configuration n segments gap) =
    let
        line y =
            makePath segments 0 y 110 y
    in
    List.map line <|
        List.Extra.initialize n (toFloat << (*) gap)



-- Composes the generative functions together and draw on screen


view : Model -> Html Msg
view model =
    case model of
        Model (Configuration n semgents gap) lines ->
            let
                data =
                    setup (Configuration n semgents gap)

                randomValues =
                    List.map (List.map ((*) (toFloat gap))) lines

                transformed =
                    List.map2 (map2Second (+)) randomValues data
            in
            a4Landscape
                []
                (paths <| List.map (translateList 90 50) transformed)

        _ ->
            text ""



-- Update is where randomness happens


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Generate, Setup (Configuration n segments gap) ) ->
            ( model
            , Random.generate Draw <|
                randomList2 n segments
            )

        ( Draw data, Setup config ) ->
            ( Model config data, Cmd.none )

        _ ->
            ( model, Cmd.none )
