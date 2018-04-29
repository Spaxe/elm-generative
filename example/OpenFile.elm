module Example.OpenFile exposing (..)

{-| Open a SVG from file for printing
-}

import Html exposing (Html, div, text)
import Draw exposing (..)


-- Model contains the generative/random element


type alias Model =
    {}



-- Msg setup the drawing process


type Msg
    = AwaitUpload
    | Print



-- Init just calls the first update


init : ( Model, Cmd Msg )
init =
    update AwaitUpload {}



-- Composes the generative functions together and draw on screen


view : Model -> Html Msg
view model =
    a4Landscape
        []
        []



-- Update is where randomness happens


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AwaitUpload ->
            ( model, Cmd.none )

        Print ->
            ( model, Cmd.none )
