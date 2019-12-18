port module Main exposing (main)

{-| Single Gallery Application to display various examples of generative art.
-}

import Browser exposing (Document)
import Browser.Navigation as Nav
import Css exposing (..)
import Example.Chords as Chords
import Example.Crescent as Crescent
import Example.Curtain as Curtain
import Example.Grid as Grid
import Example.HilbertCurve as HilbertCurve
import Example.Landscape as Landscape
import Example.ParallelRandom as ParallelRandom
import Example.Rectangles as Rectangles
import Example.Sun as Sun
import Generative exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, href, id, src, type_)
import Html.Styled.Events exposing (onClick)
import Json.Decode exposing (decodeString, errorToString, field, string)
import Ports exposing (disableMotor, download, getPlotterStatus, print, raiseLowerPen)
import Tuple exposing (first, mapFirst, mapSecond, second)
import Url
import Url.Parser
    exposing
        ( Parser
        , oneOf
        , parse
        , top
        )



-- MODEL --


type Action
    = RaiseLowerPen
    | DisableMotor
    | Print
    | Download


type Route
    = Crescent Crescent.Model
    | Grid Grid.Model
    | Curtain Curtain.Model
    | Landscape Landscape.Model
    | ParallelRandom ParallelRandom.Model
    | Sun Sun.Model
    | Chords Chords.Model
    | Rectangles Rectangles.Model
    | HilbertCurve HilbertCurve.Model


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Maybe Route
    , status : String
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | CrescentMsg Crescent.Msg
    | GridMsg Grid.Msg
    | ParallelRandomMsg ParallelRandom.Msg
    | CurtainMsg Curtain.Msg
    | LandscapeMsg Landscape.Msg
    | SunMsg Sun.Msg
    | ChordsMsg Chords.Msg
    | RectanglesMsg Rectangles.Msg
    | HilbertCurveMsg HilbertCurve.Msg
    | Menu Action
    | PlotterStatus String



-- UPDATE --


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    initRoute <| Model key url Nothing ""


initRoute : Model -> ( Model, Cmd Msg )
initRoute model =
    case model.url.fragment of
        Just "crescent" ->
            Crescent.init
                |> mapTuple2 (\m -> { model | route = Just (Crescent m) }) (Cmd.map CrescentMsg)

        Just "grid" ->
            Grid.init
                |> mapTuple2 (\m -> { model | route = Just (Grid m) }) (Cmd.map GridMsg)

        Just "parallel-random" ->
            ParallelRandom.init
                |> mapTuple2 (\m -> { model | route = Just (ParallelRandom m) }) (Cmd.map ParallelRandomMsg)

        Just "curtain" ->
            Curtain.init
                |> mapTuple2 (\m -> { model | route = Just (Curtain m) }) (Cmd.map CurtainMsg)

        Just "landscape" ->
            Landscape.init
                |> mapTuple2 (\m -> { model | route = Just (Landscape m) }) (Cmd.map LandscapeMsg)

        Just "sun" ->
            Sun.init
                |> mapTuple2 (\m -> { model | route = Just (Sun m) }) (Cmd.map SunMsg)

        Just "chords" ->
            Chords.init
                |> mapTuple2 (\m -> { model | route = Just (Chords m) }) (Cmd.map ChordsMsg)

        Just "rectangles" ->
            Rectangles.init
                |> mapTuple2 (\m -> { model | route = Just (Rectangles m) }) (Cmd.map RectanglesMsg)

        Just "hilbert-curve" ->
            HilbertCurve.init
                |> mapTuple2 (\m -> { model | route = Just (HilbertCurve m) }) (Cmd.map HilbertCurveMsg)

        Just _ ->
            ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            initRoute { model | url = url }

        ( Menu action, _ ) ->
            case action of
                RaiseLowerPen ->
                    ( model, raiseLowerPen "" )

                DisableMotor ->
                    ( model, disableMotor "" )

                Print ->
                    ( model, print "" )

                Download ->
                    ( model, download <| Maybe.withDefault "" model.url.fragment )

        ( PlotterStatus value, _ ) ->
            ( { model | status = decodePlotterStatus value }, Cmd.none )

        ( subMsg, { route } ) ->
            case ( subMsg, route ) of
                ( CrescentMsg pageMsg, Just (Crescent pageModel) ) ->
                    Crescent.update pageMsg pageModel
                        |> mapTuple2 (\m -> { model | route = Just (Crescent m) }) (Cmd.map CrescentMsg)

                ( GridMsg pageMsg, Just (Grid pageModel) ) ->
                    Grid.update pageMsg pageModel
                        |> mapTuple2 (\m -> { model | route = Just (Grid m) }) (Cmd.map GridMsg)

                ( _, _ ) ->
                    ( model, Cmd.none )



-- VIEW --


view : Model -> Document Msg
view model =
    Document "Functional Generative Art"
        [ toUnstyled <|
            div
                [ css
                    [ displayFlex
                    , fontFamilies [ "sans-serif" ]
                    , height (vh 100)
                    ]
                ]
                [ nav
                    [ css
                        [ flex3 (int 0) (int 0) (rem 10)
                        , boxSizing borderBox
                        , height (vh 100)
                        , backgroundColor (hex "F5F5F6")
                        , position relative
                        , padding2 (rem 1) (rem 2)
                        , fontSize (pct 80)
                        , lineHeight (num 1.5)
                        ]
                    ]
                    [ strong [] [ text "Generative Art Examples" ]
                    , a
                        [ href "https://twitter.com/Xavier_Ho"
                        , css
                            [ color (hex "acacac")
                            , marginTop (rem 1)
                            , display block
                            , textDecoration none
                            ]
                        ]
                        [ text "@Xavier_Ho" ]
                    , p [] [ text "Repetition" ]
                    , a [ href "#grid", css menuLinkStyles ] [ text "Grid" ]
                    , a [ href "#crescent", css menuLinkStyles ] [ text "Crescent" ]
                    , p [] [ text "Accumulation" ]
                    , a [ href "#parallel-random", css menuLinkStyles ] [ text "Parallel Random" ]
                    , a [ href "#curtain", css menuLinkStyles ] [ text "Curtain" ]
                    , a [ href "#landscape", css menuLinkStyles ] [ text "Landscape" ]
                    , a [ href "#sun", css menuLinkStyles ] [ text "Sun" ]
                    , a [ href "#chords", css menuLinkStyles ] [ text "Chords" ]
                    , p [] [ text "L-Systems" ]
                    , a [ href "#rectangles", css menuLinkStyles ] [ text "Rectangles" ]
                    , a [ href "#hilbert-curve", css menuLinkStyles ] [ text "Hilbert Curve" ]
                    ]
                , main_
                    [ css
                        [ property "flex" "1 0 auto"
                        , position relative
                        , displayFlex
                        , flexDirection column
                        ]
                    ]
                    [ div
                        [ css
                            [ flex3 (int 0) (int 0) (rem 2)
                            , fontSize (pct 70)
                            , displayFlex
                            , alignItems center
                            , backgroundColor (hex "f3f3f3")
                            ]
                        ]
                        [ div
                            [ css [ margin (rem 1), fontSize initial, color (hex "aaa") ] ]
                            [ text <| "axidraw V3 " ]
                        , button
                            [ onClick (Menu RaiseLowerPen), css menuButtonstyles ]
                            [ text "↕️ Raise/Lower pen" ]
                        , button
                            [ onClick (Menu DisableMotor), css menuButtonstyles ]
                            [ text "🚫 Disable motor" ]
                        , button
                            [ onClick (Menu Print), css menuButtonstyles ]
                            [ text "✎ Print" ]
                        , input
                            [ id "svgFile"
                            , type_ "file"
                            , css [ property "display" "none" ]
                            ]
                            []
                        , button
                            [ onClick (Menu Download), css menuButtonstyles ]
                            [ text "💾 Download" ]
                        , div
                            [ css [ margin (rem 1), fontSize initial, color (hex "dedede") ] ]
                            [ text <| model.status ]
                        ]
                    , div
                        [ css
                            [ property "flex" "1 0 auto"
                            , displayFlex
                            , position relative
                            , justifyContent center
                            , alignItems center
                            , backgroundColor (hex "efefef")
                            ]
                        ]
                        (case model.route of
                            Just route ->
                                [ render route ]

                            Nothing ->
                                []
                        )
                    ]
                ]
        ]


menuLinkStyles : List Style
menuLinkStyles =
    [ color (hex "9af")
    , cursor pointer
    , display block
    , textDecoration none
    , hover
        [ color (hex "67c")
        ]
    ]


menuButtonstyles : List Style
menuButtonstyles =
    [ backgroundColor transparent
    , border (px 0)
    , color (hex "9af")
    , margin (px 0)
    , padding (rem 1)
    , cursor pointer
    , hover
        [ color (hex "67c")
        , backgroundColor (hex "efefef")
        ]
    ]


render : Route -> Html Msg
render r =
    fromUnstyled <|
        case r of
            Crescent pageModel ->
                Crescent.view pageModel
                    |> Html.map CrescentMsg

            Grid pageModel ->
                Grid.view pageModel
                    |> Html.map GridMsg

            ParallelRandom pageModel ->
                ParallelRandom.view pageModel
                    |> Html.map ParallelRandomMsg

            Curtain pageModel ->
                Curtain.view pageModel
                    |> Html.map CurtainMsg

            Landscape pageModel ->
                Landscape.view pageModel
                    |> Html.map LandscapeMsg

            Sun pageModel ->
                Sun.view pageModel
                    |> Html.map SunMsg

            Chords pageModel ->
                Chords.view pageModel
                    |> Html.map ChordsMsg

            Rectangles pageModel ->
                Rectangles.view pageModel
                    |> Html.map RectanglesMsg

            HilbertCurve pageModel ->
                HilbertCurve.view pageModel
                    |> Html.map HilbertCurveMsg


decodePlotterStatus : String -> String
decodePlotterStatus value =
    case decodeString (field "version" string) value of
        Ok s ->
            s

        Err e ->
            errorToString e



-- PORTS --


port getPlotterStatus : (String -> msg) -> Sub msg


port raiseLowerPen : String -> Cmd msg


port disableMotor : String -> Cmd msg


port print : String -> Cmd msg


port download : String -> Cmd msg



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--Sub.batch
--    [ getPlotterStatus PlotterStatus
--    ]
-- ROUTING --
-- ENTRY --


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
