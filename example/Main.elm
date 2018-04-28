port module Main exposing (main)

{-| Single Gallery Application to display various examples of generative art.

@docs main

-}

import Navigation exposing (Location)
import Html exposing (Html, div, text, main_, nav, node, article, a, p, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (href, class)
import Tuple exposing (first, mapFirst, mapSecond)
import UrlParser exposing (..)


-- elm-generative examples

import Example.Template as Template
import Example.Curtain as Curtain
import Example.Landscape as Landscape


-- MODEL --


init : Location -> ( Route, Cmd Msg )
init location =
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


type Msg
    = NavigateTo Location
    | Menu Action
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



-- VIEW --


view : Route -> Html Msg
view route =
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
                , button
                    [ onClick (Menu Download) ]
                    [ text "💾 Download" ]
                ]
            , div
                [ class "main" ]
                [ render route ]
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


update : Msg -> Route -> ( Route, Cmd Msg )
update msg route =
    case ( msg, route ) of
        ( NavigateTo location, _ ) ->
            init location

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

        ( Menu action, _ ) ->
            case action of
                RaiseLowerPen ->
                    ( route, raiseLowerPen 6743 )

                DisableMotor ->
                    ( route, disableMotor 6743 )

                Print ->
                    ( route, print 6743 )

                Download ->
                    ( route, download <| toString route )

        _ ->
            ( route, Cmd.none )



-- PORTS --


port raiseLowerPen : Int -> Cmd msg


port disableMotor : Int -> Cmd msg


port print : Int -> Cmd msg


port download : String -> Cmd msg



-- SUBSCRIPTIONS --


subscriptions : Route -> Sub Msg
subscriptions model =
    Sub.none



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
main : Program Never Route Msg
main =
    Navigation.program NavigateTo
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
