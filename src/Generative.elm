module Generative
    exposing
        ( accumulate
        , accumulateList
        , accumulateTuple
        , accumulateTupleList
        , firstList
        , makePath
        , map2First
        , map2Second
        , map2Tuple
        , random
        , randomList
        , randomTuple
        , secondList
        , translate
        , translateList
        , transpose
        , updateFirst
        , updateSecond
        )

{-| Tools to help you tinker.


# Random generators

@docs random, randomTuple, randomList


# Shape creators

@docs makePath


# Shape transformers

@docs accumulate, accumulateTuple, accumulateList, accumulateTupleList, translate, translateList


# Utils

@docs firstList, secondList, map2First, map2Second, map2Tuple, updateFirst, updateSecond

@docs transpose

-}

import List.Extra exposing (scanl1, zip)
import Random exposing (Generator)
import Tuple exposing (..)


{-| Generates a float between -0.5 and 0.5.
-}
random : Generator Float
random =
    Random.float -0.5 0.5


{-| Generates one random tuple between (-0.5, -0.5) and (0.5, 0.5)
-}
randomTuple : Generator ( Float, Float )
randomTuple =
    Random.pair random random


{-| Generates a list of random amplitudes
-}
randomList : Int -> Generator (List Float)
randomList n =
    Random.list n random


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
                (x2 - x1) / toFloat n

            dy =
                (y2 - y1) / toFloat n

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


{-| Adds every tuple to the next tuple so far and keep a running sum.
-}
accumulateTuple : List ( number, number ) -> List ( number, number )
accumulateTuple =
    scanl1 (\a b -> ( first a + first b, second a + second b ))


{-| Adds every list to the next list so far and keep a running sum.
-}
accumulateList : List (List number) -> List (List number)
accumulateList =
    scanl1 <| List.map2 (+)


{-| Adds every list of tuples to the next list of tuples so far and keep a running sum.
-}
accumulateTupleList : List (List ( number, number )) -> List (List ( number, number ))
accumulateTupleList =
    scanl1 <| List.map2 (\a b -> ( first a + first b, second a + second b ))


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


{-| Map a function over a List of Tuples to a List of Tuples, and return a new list of tuples
-}
map2Tuple : (a -> a -> a1) -> List ( a, a ) -> List ( a, a ) -> List ( a1, a1 )
map2Tuple f =
    List.map2 (\a b -> ( f (first a) (first b), f (second a) (second b) ))


{-| transpose a 2D list
-}
transpose : List (List a) -> List (List a)
transpose =
    List.Extra.transpose
