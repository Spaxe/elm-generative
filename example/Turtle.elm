module Example.Turtle exposing (..)

import Draw exposing (..)
import LSystem
import List.Extra exposing (mapAccuml)
import Svg.PathD as PathD exposing (Segment(..), d_)
import Svg exposing (Svg)
import Svg.Attributes exposing (transform)
import Html exposing (Html, text)


type Model
    = Model Int (List State)


type Configuration
    = Configuration ( Float, Float ) Float


type State
    = D -- | Draw forward
    | S -- | Skip foward without drawing
    | L -- | Turn left by a degrees
    | R -- | Turn right by a degrees


rule : LSystem.Rule State
rule state =
    case state of
        D ->
            [ D, D, R, D, L, D, R, D, R, D, D ]

        S ->
            [ S ]

        L ->
            [ L ]

        R ->
            [ R ]


type Msg
    = Iterate
    | Draw


init : ( Model, Cmd Msg )
init =
    update Iterate (Model 4 [ D, R, D, R, D, R, D ])


draw : Model -> Configuration -> Svg Msg
draw (Model _ states) (Configuration p0 a0) =
    Svg.path
        [ d_ <| [ PathD.M p0 ] ++ turtle states (Configuration p0 a0)
        , Svg.Attributes.strokeWidth "0.2"
        ]
        []


turtle : List State -> Configuration -> List Segment
turtle states (Configuration p0 a0) =
    let
        next ( x, y ) a =
            ( x + cos (degrees a), y + sin (degrees a) )

        f ( p, a ) state =
            case state of
                D ->
                    ( ( next p a, a ), [ PathD.L (next p a) ] )

                S ->
                    ( ( next p a, a ), [ PathD.M (next p a) ] )

                L ->
                    ( ( p, a - 90 ), [] )

                R ->
                    ( ( p, a + 90 ), [] )
    in
        List.concat (Tuple.second <| mapAccuml f ( p0, a0 ) states)


view : Model -> Html Msg
view model =
    case model of
        Model n state ->
            a4Landscape
                []
                [ g
                    [ transform <| Draw.translate 125 70 ++ Draw.scale 2 ]
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
