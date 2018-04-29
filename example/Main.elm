port module Main exposing (main)

{-| Single Gallery Application to display various examples of generative art.

@docs main

-}

import Json.Decode exposing (field, string, decodeString)
import Navigation exposing (Location)
import Html exposing (Html, div, text, main_, nav, node, article, a, p, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (href, class)
import Tuple exposing (first, second, mapFirst, mapSecond)
import UrlParser exposing (s, parseHash, Parser, oneOf, top)


-- elm-generative examples

import Example.OpenFile as OpenFile
import Example.Curtain as Curtain
import Example.Landscape as Landscape
import Example.ParallelRandom as ParallelRandom


-- MODEL --


init : Location -> ( Model, Cmd Msg )
init location =
    let
        routeMsg =
            case parseLocation location of
                OpenFile _ ->
                    OpenFile.init
                        |> mapFirst (OpenFile << Just)
                        |> mapSecond (Cmd.map OpenFileMsg)

                Curtain _ ->
                    Curtain.init
                        |> mapFirst (Curtain << Just)
                        |> mapSecond (Cmd.map CurtainMsg)

                Landscape _ ->
                    Landscape.init
                        |> mapFirst (Landscape << Just)
                        |> mapSecond (Cmd.map LandscapeMsg)

                ParallelRandom _ ->
                    ParallelRandom.init
                        |> mapFirst (ParallelRandom << Just)
                        |> mapSecond (Cmd.map ParallelRandomMsg)
    in
        ( { route = first routeMsg
          , status = Nothing
          }
        , second routeMsg
        )


type Msg
    = NavigateTo Location
    | Menu Action
    | PlotterStatus String
    | OpenFileMsg OpenFile.Msg
    | CurtainMsg Curtain.Msg
    | LandscapeMsg Landscape.Msg
    | ParallelRandomMsg ParallelRandom.Msg


type Action
    = RaiseLowerPen
    | DisableMotor
    | Print
    | Download


type Route
    = OpenFile (Maybe OpenFile.Model)
    | Curtain (Maybe Curtain.Model)
    | Landscape (Maybe Landscape.Model)
    | ParallelRandom (Maybe ParallelRandom.Model)


type alias Model =
    { route : Route
    , status : Maybe String
    }



-- VIEW --


view : Model -> Html Msg
view model =
    main_
        []
        [ nav
            []
            [ p [] [ text "Filesystem" ]
            , a [ href "/#open-file" ] [ text "Open File" ]
            , p [] [ text "Accumulation" ]
            , a [ href "/#curtain" ] [ text "Curtain" ]
            , a [ href "/#parallel-random" ] [ text "Parallel Random" ]
            , a [ href "/#landscape" ] [ text "Landscape" ]
            ]
        , article
            []
            [ div
                [ class "options" ]
                [ button
                    [ onClick (Menu RaiseLowerPen) ]
                    [ text "↕️ Raise/Lower" ]
                , button
                    [ onClick (Menu DisableMotor) ]
                    [ text "\x1F6D1 Disable motor" ]
                , button
                    [ onClick (Menu Print) ]
                    [ text "🖊 Print" ]
                , div
                    [ class "status" ]
                    [ text <| Maybe.withDefault "" model.status ]
                , button
                    [ onClick (Menu Download) ]
                    [ text "💾 Download" ]
                ]
            , div
                [ class "main" ]
                [ render model.route ]
            ]
        ]


render : Route -> Html Msg
render route =
    case route of
        OpenFile (Just pageModel) ->
            OpenFile.view pageModel
                |> Html.map OpenFileMsg

        Curtain (Just pageModel) ->
            Curtain.view pageModel
                |> Html.map CurtainMsg

        Landscape (Just pageModel) ->
            Landscape.view pageModel
                |> Html.map LandscapeMsg

        ParallelRandom (Just pageModel) ->
            ParallelRandom.view pageModel
                |> Html.map ParallelRandomMsg

        _ ->
            text "404 Not Found"



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.route ) of
        ( NavigateTo location, _ ) ->
            init location

        ( Menu action, _ ) ->
            case action of
                RaiseLowerPen ->
                    ( model, raiseLowerPen "" )

                DisableMotor ->
                    ( model, disableMotor "" )

                Print ->
                    ( model, print "" )

                Download ->
                    ( model, download <| toString model.route )

        ( PlotterStatus value, _ ) ->
            ( { model | status = Just <| decodePlotterStatus value }, Cmd.none )

        ( msg, route ) ->
            let
                routeMsg =
                    case ( msg, route ) of
                        ( OpenFileMsg pageMsg, OpenFile (Just pageModel) ) ->
                            OpenFile.update pageMsg pageModel
                                |> mapFirst (OpenFile << Just)
                                |> mapSecond (Cmd.map OpenFileMsg)

                        ( CurtainMsg pageMsg, Curtain (Just pageModel) ) ->
                            Curtain.update pageMsg pageModel
                                |> mapFirst (Curtain << Just)
                                |> mapSecond (Cmd.map CurtainMsg)

                        ( LandscapeMsg pageMsg, Landscape (Just pageModel) ) ->
                            Landscape.update pageMsg pageModel
                                |> mapFirst (Landscape << Just)
                                |> mapSecond (Cmd.map LandscapeMsg)

                        ( ParallelRandomMsg pageMsg, ParallelRandom (Just pageModel) ) ->
                            ParallelRandom.update pageMsg pageModel
                                |> mapFirst (ParallelRandom << Just)
                                |> mapSecond (Cmd.map ParallelRandomMsg)

                        _ ->
                            ( route, Cmd.none )
            in
                ( { model | route = first routeMsg }, Cmd.none )


decodePlotterStatus : String -> String
decodePlotterStatus value =
    case decodeString (field "version" string) value of
        Ok s ->
            s

        Err e ->
            toString e



-- PORTS --


port getPlotterStatus : (String -> msg) -> Sub msg


port raiseLowerPen : String -> Cmd msg


port disableMotor : String -> Cmd msg


port print : String -> Cmd msg


port download : String -> Cmd msg



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions route =
    Sub.batch
        [ getPlotterStatus PlotterStatus
        ]



-- ROUTING --


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ UrlParser.map (Curtain Nothing) top
        , UrlParser.map (OpenFile Nothing) (s "open-file")
        , UrlParser.map (Curtain Nothing) (s "curtain")
        , UrlParser.map (Landscape Nothing) (s "landscape")
        , UrlParser.map (ParallelRandom Nothing) (s "parallel-random")
        ]


parseLocation : Location -> Route
parseLocation location =
    case parseHash matchers location of
        Just route ->
            route

        Nothing ->
            Curtain Nothing


{-| Program Entry.
-}
main : Program Never Model Msg
main =
    Navigation.program NavigateTo
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
