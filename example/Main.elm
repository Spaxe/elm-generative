port module Main exposing (main)

{-| Single Gallery Application to display various examples of generative art.

@docs main

-}

import Navigation exposing (Location)
import Html exposing (Html, div, text, main_, nav, node, article, a, p, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (href, class)
import Tuple exposing (first, second, mapFirst, mapSecond)
import UrlParser exposing (..)


-- elm-generative examples

import Example.Template as Template
import Example.Curtain as Curtain
import Example.Landscape as Landscape


-- MODEL --


init : Location -> ( Model, Cmd Msg )
init location =
    let
        routeMsg =
            case parseLocation location of
                Curtain _ ->
                    Curtain.init
                        |> mapFirst (Curtain << Just)
                        |> mapSecond (Cmd.map CurtainMsg)

                Landscape _ ->
                    Landscape.init
                        |> mapFirst (Landscape << Just)
                        |> mapSecond (Cmd.map LandscapeMsg)

                Template _ ->
                    Template.init
                        |> mapFirst (Template << Just)
                        |> mapSecond (Cmd.map TemplateMsg)
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
    | CurtainMsg Curtain.Msg
    | LandscapeMsg Landscape.Msg
    | TemplateMsg Template.Msg


type Action
    = RaiseLowerPen
    | DisableMotor
    | Print
    | Download


type Route
    = Curtain (Maybe Curtain.Model)
    | Landscape (Maybe Landscape.Model)
    | Template (Maybe Template.Model)


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
            [ p [] [ text "Make your own" ]
            , a [ href "/#template" ] [ text "Template" ]
            , p [] [ text "Accumulation" ]
            , a [ href "/#curtain" ] [ text "Curtain" ]
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
        Curtain (Just pageModel) ->
            Curtain.view pageModel
                |> Html.map CurtainMsg

        Landscape (Just pageModel) ->
            Landscape.view pageModel
                |> Html.map LandscapeMsg

        Template (Just pageModel) ->
            Template.view pageModel
                |> Html.map TemplateMsg

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

        ( PlotterStatus s, _ ) ->
            ( model, Cmd.none )

        ( msg, route ) ->
            let
                routeMsg =
                    case ( msg, route ) of
                        ( CurtainMsg pageMsg, Curtain (Just pageModel) ) ->
                            Curtain.update pageMsg pageModel
                                |> mapFirst (Curtain << Just)
                                |> mapSecond (Cmd.map CurtainMsg)

                        ( LandscapeMsg pageMsg, Landscape (Just pageModel) ) ->
                            Landscape.update pageMsg pageModel
                                |> mapFirst (Landscape << Just)
                                |> mapSecond (Cmd.map LandscapeMsg)

                        ( TemplateMsg pageMsg, Template (Just pageModel) ) ->
                            Template.update pageMsg pageModel
                                |> mapFirst (Template << Just)
                                |> mapSecond (Cmd.map TemplateMsg)

                        _ ->
                            ( route, Cmd.none )
            in
                ( { model | route = first routeMsg }, Cmd.none )



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
        [ map (Template Nothing) top
        , map (Template Nothing) (s "template")
        , map (Curtain Nothing) (s "curtain")
        , map (Landscape Nothing) (s "landscape")
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
