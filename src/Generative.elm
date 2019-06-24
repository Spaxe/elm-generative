module Generative exposing
    ( random, randomTuple, randomList, randomList2, randomListTuple, randomListTuple2
    , makePath, makeGrid
    , accumulate, accumulateTuple, accumulateList, accumulateListTuple, translate, translateList
    , firstList, secondList, mapList, mapList2, map2First, map2Second, map2Tuple, mapFirstList, mapSecondList, mapTuple, mapTuple2
    )

{-| Tools to help you tinker.


# Random generators

@docs random, randomTuple, randomList, randomList2, randomListTuple, randomListTuple2


# Shape creators

@docs makePath, makeGrid


# Shape transformers

@docs accumulate, accumulateTuple, accumulateList, accumulateListTuple, translate, translateList


# Utils

@docs firstList, secondList, mapList, mapList2, map2First, map2Second, map2Tuple, mapFirstList, mapSecondList, mapTuple, mapTuple2

-}

import List.Extra exposing (scanl1, zip)
import Random exposing (Generator)
import Tuple exposing (..)



-- GENERATORS --


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


{-| Generates a list of random floats
-}
randomList : Int -> Generator (List Float)
randomList n =
    Random.list n random


{-| Generates a nested list of random floats
-}
randomList2 : Int -> Int -> Generator (List (List Float))
randomList2 m n =
    Random.list m <| Random.list n random


{-| Generates a list of random tuples
-}
randomListTuple : Int -> Generator (List ( Float, Float ))
randomListTuple n =
    Random.list n randomTuple


{-| Generates a list of lists of random tuples
-}
randomListTuple2 : Int -> Int -> Generator (List (List ( Float, Float )))
randomListTuple2 m n =
    Random.list n <| randomListTuple n



-- OPERATIONS ON A SINGLE LIST --


{-| Maps a function over a list of lists
-}
mapList : (a -> b) -> List (List a) -> List (List b)
mapList =
    List.map << List.map


{-| Maps a function over 2 lists of lists
-}
mapList2 : (a -> b -> c) -> List (List a) -> List (List b) -> List (List c)
mapList2 =
    List.map2 << List.map2


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
accumulateListTuple : List (List ( number, number )) -> List (List ( number, number ))
accumulateListTuple =
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
mapFirstList : (a -> a1) -> List ( a, b ) -> List ( a1, b )
mapFirstList f =
    List.map (mapFirst f)


{-| Change the second value in a list of tuples
-}
mapSecondList : (b -> b1) -> List ( a, b ) -> List ( a, b1 )
mapSecondList f =
    List.map (mapSecond f)



-- OPERATIONS ON TUPLE(S) --


{-| Maps over a tuple
-}
mapTuple : (a -> a1) -> ( a, a ) -> ( a1, a1 )
mapTuple f =
    mapFirst f << mapSecond f


{-| Maps over a tuple using two functions for two components
-}
mapTuple2 : (a -> a1) -> (b -> b1) -> ( a, b ) -> ( a1, b1 )
mapTuple2 f g =
    mapFirst f << mapSecond g


{-| Moves a point
-}
translate : number -> number -> ( number, number ) -> ( number, number )
translate x y =
    mapTuple2 ((+) x) ((+) y)


{-| Moves a list of points
-}
translateList : number -> number -> List ( number, number ) -> List ( number, number )
translateList x y =
    mapFirstList ((+) x) << mapSecondList ((+) y)


{-| Maps a function over a List to the first element of List of Tuples, and
return a new list only modifying the first values.
-}
map2First : (a -> a -> a1) -> List a -> List ( a, b ) -> List ( a1, b )
map2First f =
    List.map2 <| mapFirst << f


{-| Maps a function over a List to the second element of List of Tuples, and
return a new list only modifying the second values.
-}
map2Second : (a -> a -> a1) -> List a -> List ( b, a ) -> List ( b, a1 )
map2Second f =
    List.map2 <| mapSecond << f


{-| Maps a function over a List of Tuples to a List of Tuples, and return a new list of tuples
-}
map2Tuple : (a -> a -> a1) -> List ( a, a ) -> List ( a, a ) -> List ( a1, a1 )
map2Tuple f =
    List.map2 (\a b -> ( f (first a) (first b), f (second a) (second b) ))



-- LIST CREATIONS --


{-| Creates a 2D grid of size m x n, with diagnoals at (0, 0) and (m-1, n-1).
-}
makeGrid : Int -> Int -> List ( Float, Float )
makeGrid m n =
    List.Extra.initialize m toFloat
        |> List.Extra.andThen
            (\x ->
                List.Extra.initialize n toFloat
                    |> List.Extra.andThen (\y -> [ ( x, y ) ])
            )



-- PATH CREATIONS --


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
