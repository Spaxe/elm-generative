module Example.Rectangles exposing
    ( Model(..)
    , Msg(..)
    , init
    , initModel
    , update
    , view
    )

import Draw exposing (..)
import Html exposing (Html, text)
import LSystem
import LSystem.Turtle exposing (State(..), turtle)
import Svg exposing (Svg)
import Svg.Attributes exposing (d, transform)
import Svg.PathD as PathD exposing (pathD)


type Model
    = Model Int (List State)


type Configuration
    = Configuration ( Float, Float ) Float


rule : LSystem.Rule State
rule state =
    case state of
        D ->
            [ D, D, R, D, L, D, R, D, R, D, D ]

        s ->
            [ s ]


type Msg
    = Iterate


init : ( Model, Cmd Msg )
init =
    update Iterate initModel


initModel : Model
initModel =
    Model 3 [ D, R, D, R, D, R, D ]


draw : Model -> Configuration -> Svg Msg
draw (Model _ states) (Configuration p0 a0) =
    Svg.path
        [ d <| pathD <| [ PathD.M p0 ] ++ turtle states 90
        , Svg.Attributes.strokeWidth "0.2"
        ]
        []


view : Model -> Html Msg
view model =
    case model of
        Model n state ->
            a4Landscape
                []
                [ g
                    [ transform <| Draw.translate 150 100 ++ Draw.scale 2 ]
                    [ draw model (Configuration ( 0, 0 ) 0) ]
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Iterate, Model n state ) ->
            case n > 0 of
                True ->
                    update Iterate <|
                        Model (n - 1) (LSystem.apply rule state)

                False ->
                    ( model, Cmd.none )
