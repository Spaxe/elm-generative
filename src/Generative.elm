module Generative
    exposing
        ( random
        , random1D
        , makePath
        , firstList
        , secondList
        , map2First
        , map2Second
        , updateFirst
        , updateSecond
        , transpose
        , accumulate
        , accumulateList
        , translate
        , translateList
        )

{-| Tools to help you tinker.


# Random generators

@docs random, random1D


# Shape creators

@docs makePath


# Shape transformers

@docs accumulate, accumulateList, translate, translateList


# Utils

@docs firstList, secondList, map2First, map2Second, updateFirst, updateSecond

@docs transpose

-}

import Random exposing (Generator, float, list)
import Tuple exposing (..)
import List.Extra exposing (zip, scanl1)


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


{-| Adds every number in the list so far and keep a running sum.
-}
accumulate : List number -> List number
accumulate =
    scanl1 (+)


{-| Adds every list to the next list so far and keep a running sum.
-}
accumulateList : List (List number) -> List (List number)
accumulateList =
    scanl1 <| List.map2 (+)


{-| Extracts the first elements of a list of tuples
-}
firstList : List ( a, b ) -> List a
firstList =
    List.map first


{-| Extracts the second elements of a list of tuples
-}
secondList : List ( a, b ) -> List b
secondList =
    List.map second


{-| Change the first value in a list of tuples
-}
updateFirst : (a -> a1) -> List ( a, b ) -> List ( a1, b )
updateFirst f =
    List.map (mapFirst f)


{-| Change the second value in a list of tuples
-}
updateSecond : (b -> b1) -> List ( a, b ) -> List ( a, b1 )
updateSecond f =
    List.map (mapSecond f)


{-| Move a point
-}
translate : number -> number -> ( number, number ) -> ( number, number )
translate x y point =
    point
        |> mapFirst ((+) x)
        |> mapSecond ((+) y)


{-| Move a list of points
-}
translateList : number -> number -> List ( number, number ) -> List ( number, number )
translateList x y points =
    points
        |> updateFirst ((+) x)
        |> updateSecond ((+) y)


{-| Map a function over a List to the first element of List of Tuples, and
return a new list only modifying the first values.
-}
map2First : (a -> a -> a1) -> List a -> List ( a, b ) -> List ( a1, b )
map2First f =
    List.map2 <| mapFirst << f


{-| Map a function over a List to the second element of List of Tuples, and
return a new list only modifying the second values.
-}
map2Second : (a -> a -> a1) -> List a -> List ( b, a ) -> List ( b, a1 )
map2Second f =
    List.map2 <| mapSecond << f


{-| transpose a 2D list
-}
transpose : List (List a) -> List (List a)
transpose =
    List.Extra.transpose
