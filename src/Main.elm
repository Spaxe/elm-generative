port module Main exposing (main)

{-| Single Gallery Application to display various examples of generative art.

@docs main

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
import Html.Styled.Attributes exposing (class, css, href, src)
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


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    update (UrlChanged url) (Model key (Crescent Nothing) "")



-- MODEL --


type Action
    = RaiseLowerPen
    | DisableMotor
    | Print
    | Download


type Route
    = Crescent (Maybe Crescent.Model)
    | Grid (Maybe Grid.Model)
    | Curtain (Maybe Curtain.Model)
    | Landscape (Maybe Landscape.Model)
    | ParallelRandom (Maybe ParallelRandom.Model)
    | Sun (Maybe Sun.Model)
    | Chords (Maybe Chords.Model)
    | Rectangles (Maybe Rectangles.Model)
    | HilbertCurve (Maybe HilbertCurve.Model)


type alias Model =
    { key : Nav.Key
    , route : Route
    , status : String
    }


type Msg
    = Homepage
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | CrescentMsg Crescent.Msg



--| Menu Action
--| PlotterStatus String
--
-- UPDATE --


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
            case parse routeParser url of
                Just (Crescent _) ->
                    let
                        routeMsgModel =
                            Crescent.init
                    in
                    ( { model | route = Crescent <| Just (Tuple.first routeMsgModel) }
                    , Cmd.map CrescentMsg <| Tuple.second routeMsgModel
                    )

                Just _ ->
                    ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ( subMsg, { route } ) ->
            case ( subMsg, route ) of
                ( CrescentMsg pageMsg, Crescent state ) ->
                    mapTuple2 (\m -> { model | route = Crescent (Just m) }) (Cmd.map CrescentMsg) <|
                        case state of
                            Nothing ->
                                Crescent.init

                            Just pageModel ->
                                Crescent.update pageMsg pageModel

                ( _, _ ) ->
                    ( model, Cmd.none )



--( Menu action, _ ) ->
--    case action of
--        RaiseLowerPen ->
--            ( model, raiseLowerPen "" )
--        DisableMotor ->
--            ( model, disableMotor "" )
--        Print ->
--            ( model, print "" )
--        Download ->
--            ( model, download <| fromRoute model.route )
--( PlotterStatus value, _ ) ->
--    ( { model | status = Just <| decodePlotterStatus value }, Cmd.none )
--( msg, r ) ->
--    let
--        routeMsg =
--            case ( msg, r ) of
--                ( CrescentMsg pageMsg, Crescent (Just pageModel) ) ->
--                    Crescent.update pageMsg pageModel
--                        |> mapTuple2 (Crescent << Just) (Cmd.map CrescentMsg)
--                ( GridMsg pageMsg, Grid (Just pageModel) ) ->
--                    Grid.update pageMsg pageModel
--                        |> mapTuple2 (Grid << Just) (Cmd.map GridMsg)
--                ( ParallelRandomMsg pageMsg, ParallelRandom (Just pageModel) ) ->
--                    ParallelRandom.update pageMsg pageModel
--                        |> mapTuple2 (ParallelRandom << Just) (Cmd.map ParallelRandomMsg)
--                ( CurtainMsg pageMsg, Curtain (Just pageModel) ) ->
--                    Curtain.update pageMsg pageModel
--                        |> mapTuple2 (Curtain << Just) (Cmd.map CurtainMsg)
--                ( LandscapeMsg pageMsg, Landscape (Just pageModel) ) ->
--                    Landscape.update pageMsg pageModel
--                        |> mapTuple2 (Landscape << Just) (Cmd.map LandscapeMsg)
--                ( SunMsg pageMsg, Sun (Just pageModel) ) ->
--                    Sun.update pageMsg pageModel
--                        |> mapTuple2 (Sun << Just) (Cmd.map SunMsg)
--                ( ChordsMsg pageMsg, Chords (Just pageModel) ) ->
--                    Chords.update pageMsg pageModel
--                        |> mapTuple2 (Chords << Just) (Cmd.map ChordsMsg)
--                ( RectanglesMsg pageMsg, Rectangles (Just pageModel) ) ->
--                    Rectangles.update pageMsg pageModel
--                        |> mapTuple2 (Rectangles << Just) (Cmd.map RectanglesMsg)
--                ( HilbertCurveMsg pageMsg, HilbertCurve (Just pageModel) ) ->
--                    HilbertCurve.update pageMsg pageModel
--                        |> mapTuple2 (HilbertCurve << Just) (Cmd.map HilbertCurveMsg)
--                _ ->
--                    ( r, Cmd.none )
--    in
--    ( { model | route = first routeMsg }, Cmd.none )
--let
--    routeMsg =
--        case routeParser url of
--            Crescent _ ->
--                Crescent.init
--                    |> mapTuple2 (Crescent << Just) (Cmd.map CrescentMsg)
--            Grid _ ->
--                Grid.init
--                    |> mapTuple2 (Grid << Just) (Cmd.map GridMsg)
--            Curtain _ ->
--                Curtain.init
--                    |> mapTuple2 (Curtain << Just) (Cmd.map CurtainMsg)
--            Landscape _ ->
--                Landscape.init
--                    |> mapTuple2 (Landscape << Just) (Cmd.map LandscapeMsg)
--            ParallelRandom _ ->
--                ParallelRandom.init
--                    |> mapTuple2 (ParallelRandom << Just) (Cmd.map ParallelRandomMsg)
--            Sun _ ->
--                Sun.init
--                    |> mapTuple2 (Sun << Just) (Cmd.map SunMsg)
--            Chords _ ->
--                Chords.init
--                    |> mapTuple2 (Chords << Just) (Cmd.map ChordsMsg)
--            Rectangles _ ->
--                Rectangles.init
--                    |> mapTuple2 (Rectangles << Just) (Cmd.map RectanglesMsg)
--            HilbertCurve _ ->
--                HilbertCurve.init
--                    |> mapTuple2 (HilbertCurve << Just) (Cmd.map HilbertCurveMsg)
--in
--( { route = first routeMsg
--  , status = Nothing
--  }
--, second routeMsg
--)
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
                    [ p [] [ text "Repetition" ]
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
                            ]
                        ]
                        [ -- button
                          --    [ onClick (Menu RaiseLowerPen), css menuButtonstyles ]
                          --    [ text "↕️ Raise/Lower pen" ]
                          --, button
                          --    [ onClick (Menu DisableMotor), css menuButtonstyles ]
                          --    [ text "🚫 Disable motor" ]
                          --, button
                          --    [ onClick (Menu Print) , css menuButtonstyles]
                          --    [ text "🖊 Print" ]
                          --, input
                          --    [ id "svgFile"
                          --    , type_ "file"
                          --    , style "display" "none"
                          --    ]
                          --    []
                          --, button
                          --    [ onClick (Menu Download), css menuButtonstyles ]
                          --    [ text "💾 Download" ]
                          div
                            [ css [ margin2 (px 0) (rem 1) ] ]
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
                        [ render model.route
                        ]
                    ]
                ]
        ]



--svg {
--    height: 100%;
--    width: auto;
--    justify-content: center;
--    align-items: center;
--    position: relative;
--    background: #FAFAFB;
--}


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
    , margin2 (rem 0) (rem 0.5)
    , padding (px 0)
    , cursor pointer
    , hover
        [ color (hex "67c")
        ]
    ]


render : Route -> Html Msg
render r =
    fromUnstyled <|
        case r of
            Crescent (Just pageModel) ->
                Crescent.view pageModel
                    |> Html.map CrescentMsg

            --Grid (Just pageModel) ->
            --    Grid.view pageModel
            --        |> Html.map GridMsg
            --ParallelRandom (Just pageModel) ->
            --    ParallelRandom.view pageModel
            --        |> Html.map ParallelRandomMsg
            --Curtain (Just pageModel) ->
            --    Curtain.view pageModel
            --        |> Html.map CurtainMsg
            --Landscape (Just pageModel) ->
            --    Landscape.view pageModel
            --        |> Html.map LandscapeMsg
            --Sun (Just pageModel) ->
            --    Sun.view pageModel
            --        |> Html.map SunMsg
            --Chords (Just pageModel) ->
            --    Chords.view pageModel
            --        |> Html.map ChordsMsg
            --Rectangles (Just pageModel) ->
            --    Rectangles.view pageModel
            --        |> Html.map RectanglesMsg
            --HilbertCurve (Just pageModel) ->
            --    HilbertCurve.view pageModel
            --        |> Html.map HilbertCurveMsg
            _ ->
                Html.text "404 Not Found"


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


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Url.Parser.map (Crescent Nothing) Url.Parser.top
        , Url.Parser.map (Grid Nothing) (Url.Parser.s "grid")
        , Url.Parser.map (Crescent Nothing) (Url.Parser.s "crescent")
        , Url.Parser.map (ParallelRandom Nothing) (Url.Parser.s "parallel-random")
        , Url.Parser.map (Curtain Nothing) (Url.Parser.s "curtain")
        , Url.Parser.map (Landscape Nothing) (Url.Parser.s "landscape")
        , Url.Parser.map (Sun Nothing) (Url.Parser.s "sun")
        , Url.Parser.map (Chords Nothing) (Url.Parser.s "chords")
        , Url.Parser.map (Rectangles Nothing) (Url.Parser.s "rectangles")
        , Url.Parser.map (HilbertCurve Nothing) (Url.Parser.s "hilbert-curve")
        ]



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
