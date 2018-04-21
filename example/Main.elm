module Main exposing (main)

{-| Single Gallery Application to display various examples of generative art.

@docs main
-}

import Html exposing (Html, div, text, main_, nav, node, article, a, p)
import Html.Events exposing (onClick)
import Tuple exposing (first, mapFirst, mapSecond)

-- elm-generative examples

import Repetition
import Shepherding


-- MODEL --


init : Route -> ( Route, Cmd Msg )
init route =
    case route of
        Repetition _ ->
            Repetition.init
                |> mapFirst (Repetition << Just)
                |> mapSecond (Cmd.map RepetitionMsg)

        Shepherding _ ->
            Shepherding.init
                |> mapFirst (Shepherding << Just)
                |> mapSecond (Cmd.map ShepherdingMsg)


type Msg
    = Set Route
    | RepetitionMsg Repetition.Msg
    | ShepherdingMsg Shepherding.Msg


type Route
    = Repetition (Maybe Repetition.Model)
    | Shepherding (Maybe Shepherding.Model)



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
            , a [ onClick <| Set (Repetition Nothing) ] [ text "Circles" ]
            , a [ onClick <| Set (Shepherding Nothing) ] [ text "Shepherding" ]
            ]
        , article
            []
            [ render route ]
        ]


render : Route -> Html Msg
render route =
    case route of
        Repetition (Just pageModel) ->
            Repetition.view pageModel
                |> Html.map RepetitionMsg

        Shepherding (Just pageModel) ->
            Shepherding.view pageModel
                |> Html.map ShepherdingMsg

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

        ( RepetitionMsg pageMsg, Repetition (Just pageModel) ) ->
            Repetition.update pageMsg pageModel
                |> mapFirst (Repetition << Just)
                |> mapSecond (Cmd.map RepetitionMsg)

        ( ShepherdingMsg pageMsg, Shepherding (Just pageModel) ) ->
            Shepherding.update pageMsg pageModel
                |> mapFirst (Shepherding << Just)
                |> mapSecond (Cmd.map ShepherdingMsg)

        _ ->
            init (Repetition Nothing)


subscriptions : Route -> Sub Msg
subscriptions model =
    Sub.none

{-| Program Entry.
-}
main : Program Never Route Msg
main =
    Html.program
        { init = init <| Repetition Nothing
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
