module Main exposing (main)

{-| Single Gallery Application to display various examples of generative art.

@docs main

-}

import Html exposing (Html, div, text, main_, nav, node, article, a, p)
import Html.Events exposing (onClick)
import Tuple exposing (first, mapFirst, mapSecond)


-- elm-generative examples

import Example.Curtain as Curtain
import Example.Landscape as Landscape


-- MODEL --


init : Route -> ( Route, Cmd Msg )
init route =
    case route of
        Curtain _ ->
            Curtain.init
                |> mapFirst (Curtain << Just)
                |> mapSecond (Cmd.map CurtainMsg)

        Landscape _ ->
            Landscape.init
                |> mapFirst (Landscape << Just)
                |> mapSecond (Cmd.map LandscapeMsg)


type Msg
    = Set Route
    | CurtainMsg Curtain.Msg
    | LandscapeMsg Landscape.Msg


type Route
    = Curtain (Maybe Curtain.Model)
    | Landscape (Maybe Landscape.Model)



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
            [ p [] [ text "Accumulation" ]
            , a [ onClick <| Set (Curtain Nothing) ] [ text "Curtain" ]
            , a [ onClick <| Set (Landscape Nothing) ] [ text "Landscape" ]
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
}

article {
    flex: 1 0 auto;
    position: relative;
    height: 100vh;
}

/* 1 cm = 1 rem */
svg {
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
        ( Set route, _ ) ->
            init route

        ( CurtainMsg pageMsg, Curtain (Just pageModel) ) ->
            Curtain.update pageMsg pageModel
                |> mapFirst (Curtain << Just)
                |> mapSecond (Cmd.map CurtainMsg)

        ( LandscapeMsg pageMsg, Landscape (Just pageModel) ) ->
            Landscape.update pageMsg pageModel
                |> mapFirst (Landscape << Just)
                |> mapSecond (Cmd.map LandscapeMsg)

        _ ->
            init (Curtain Nothing)


subscriptions : Route -> Sub Msg
subscriptions model =
    Sub.none


{-| Program Entry.
-}
main : Program Never Route Msg
main =
    Html.program
        { init = init <| Curtain Nothing
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
