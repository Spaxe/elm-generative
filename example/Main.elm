module Main exposing (main)

{-| Single Gallery Application to display various examples of generative art.

@docs main

-}

import Navigation exposing (Location)
import Html exposing (Html, div, text, main_, nav, node, article, a, p)
import Html.Attributes exposing (href)
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
    | CurtainMsg Curtain.Msg
    | LandscapeMsg Landscape.Msg
    | TemplateMsg Template.Msg


type Route
    = Curtain (Maybe Curtain.Model)
    | Landscape (Maybe Landscape.Model)
    | Template (Maybe Template.Model)



-- VIEW --


view : Route -> Html Msg
view route =
    main_
        []
        [ node "style"
            []
            [ text stylesheet ]
        , nav
            []
            [ p [] [ text "Make your own" ]
            , a [ href "/#template" ] [ text "Template" ]
            , p [] [ text "Accumulation" ]
            , a [ href "/#curtain" ] [ text "Curtain" ]
            , a [ href "/#landscape" ] [ text "Landscape" ]
            ]
        , article
            []
            [ render route ]
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


stylesheet : String
stylesheet =
    """
body {
    margin: 0;
    padding: 0;
}

main {
    width: 100vw;
    height: 100vh;
    display: flex;
}

nav {
    flex: 0 0 10rem;
    height: 100vh;
    background: #F5F5F6;
    position: relative;
    padding: 1rem 2rem;
    line-height: 1.5;
}

nav > p {
    margin-bottom: 0;
}

nav > a {
    color: #9af;
    cursor: pointer;
    display: block;
    text-decoration: none;
}

article {
    flex: 1 0 auto;
    position: relative;
    height: 100vh;
}

/* 1 cm = 1 rem */
svg {
    height: 100vh;
    width: auto;
    justify-content: center;
    align-items: center;
    position: relative;
    background: #FAFAFB;
}
"""



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

        _ ->
            ( route, Cmd.none )



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
