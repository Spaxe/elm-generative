module Example.HilbertCurve exposing (..)

import Draw exposing (..)
import LSystem
import LSystem.Turtle exposing (State(..), turtle)
import Svg.PathD as PathD exposing (d_)
import Svg exposing (Svg)
import Svg.Attributes exposing (transform)
import Html exposing (Html, text)


type Model
    = Model Int (List State)


type Configuration
    = Configuration ( Float, Float ) Float


{-| See <https://en.wikipedia.org/wiki/Hilbert_curve#Representation_as_Lindenmayer_system>
-}
rule : LSystem.Rule State
rule state =
    case state of
        A ->
            [ R, B, D, L, A, D, A, L, D, B, R ]

        B ->
            [ L, A, D, R, B, D, B, R, D, A, L ]

        s ->
            [ s ]


type Msg
    = Iterate
    | Draw


init : ( Model, Cmd Msg )
init =
    update Iterate (Model 6 [ A ])


draw : Model -> Configuration -> Svg Msg
draw (Model _ states) (Configuration p0 a0) =
    Svg.path
        [ d_ <| [ PathD.M p0 ] ++ turtle states p0 a0
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
                    [ transform <| Draw.translate 80 30 ++ Draw.scale 2 ]
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
                    update Draw model

        ( Draw, model ) ->
            ( model, Cmd.none )
