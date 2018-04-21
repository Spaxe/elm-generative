module Crawl exposing (..)

import Html exposing (Html, div, text)
import Draw exposing (..)
import Random


type alias Data =
    List (List ( Float, Float ))


type alias Model =
    { data : Data
    , state : Msg
    }


init : ( Model, Cmd Msg )
init =
    update Generate { data = [], state = Generate }


type Msg
    = Generate
    | Draw Data


view : Model -> Html Msg
view model =
    a4Landscape
        [ withStroke 0.4
        ]
        [ List.map2 path model.data [ [] ]
            |> g [ translate 10 10 ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( model
            , Random.generate Draw <|
                Random.list 1 <|
                    Random.list 2 <|
                        Random.pair (Random.float 0 100) (Random.float 0 100)
            )

        Draw data ->
            ( { model | data = data }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
