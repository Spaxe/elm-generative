module Example.Landscape exposing (..)

import Html exposing (Html, div, text)


type alias Model =
    String


init : ( Model, Cmd Msg )
init =
    ( "Shepherding Page", Cmd.none )


type Msg
    = Start


view : Model -> Html Msg
view model =
    div []
        [ text model ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
