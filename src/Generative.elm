module Generative
    exposing
        ( random
        , random1D
        , makePath
        , mapX
        , mapY
        )

{-| Tools to help you tinker.


# Random generators

@docs random, random1D


# Shape creators

@docs makePath


# Shape transformers

@docs mapX, mapY

-}

import Random exposing (Generator, float, list)
import Tuple exposing (mapFirst)
import Basics.Extra exposing (swap)
import List.Extra exposing (zip)


{-| Generates one random value at most `amplitude` apart, centred around 0.

    For example, `random 1` generates a value between `-0.5` and `0.5`.

-}
random : Float -> Generator Float
random amplitude =
    float (-amplitude / 2) (amplitude / 2)


{-| Generates a list of random amplitudes
-}
random1D : Int -> Float -> Generator (List Float)
random1D n amplitude =
    list n <| random amplitude


{-| Creates a straight line from (x1, y1) to (x2, y2) with n segments.

If n < 1, returns an empty list.

-}
makePath : Int -> Float -> Float -> Float -> Float -> List ( Float, Float )
makePath n x1 y1 x2 y2 =
    if n < 1 then
        []
    else
        let
            dx =
                (x2 - x1) / (toFloat n)

            dy =
                (y2 - y1) / (toFloat n)

            xs =
                List.range 0 n
                    |> List.map (toFloat >> (*) dx >> (+) x1)

            ys =
                List.range 0 n
                    |> List.map (toFloat >> (*) dy >> (+) y1)
        in
            zip xs ys


{-| Map a function over a List to the first element of List of Tuples, and
return a new list only modifying the first values.
-}
mapX : (a -> a -> a) -> List a -> List ( a, b ) -> List ( a, b )
mapX f a b =
    case ( a, b ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( x :: xs, xy :: xys ) ->
            mapFirst (f x) xy :: mapX f xs xys


{-| Map a function over a List to the second element of List of Tuples, and
return a new list only modifying the second values.
-}
mapY : (a -> a -> a) -> List a -> List ( b, a ) -> List ( b, a )
mapY f a b =
    List.map swap b
        |> mapX f a
        |> List.map swap
