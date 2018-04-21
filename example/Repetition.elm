module Repetition exposing (..)

import Html exposing (Html, div, text)
import Draw exposing (..)


type alias Model =
    String


init : ( Model, Cmd Msg )
init =
    ( "", Cmd.none )


type Msg
    = Start


view : Model -> Html Msg
view model =
    a4Landscape
        [ withStroke 0.4 ]
        [ g
            [ translate 10 10 ]
            [ line 0 0 5 5 [] ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
