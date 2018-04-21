module Generative
    exposing
        ( random
        , random1D
        )

{-| Tools to help you tinker.


# Random generators

@docs random, random1D


# Shape creators


# Shape transformers

-}

import Random exposing (Generator, float, list)


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


{-| Creates a line from (x1, y1) to (x2, y2) with n segments.

If n < 1, returns an empty list.

-}
line : Int -> Float -> Float -> Float -> Float -> List ( Float, Float )
