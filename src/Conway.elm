module Conway exposing (Matrix, Point, empty, tick, toggle)

import Set exposing (Set)

type alias Point = (Int, Int)
type alias Matrix = Set Point

empty : Matrix
empty = Set.empty

tick : Matrix -> Matrix
tick matrix =
    let pointsToConsider = considerPoints matrix
    in Set.foldl (tickPoint matrix) Set.empty pointsToConsider

considerPoints : Matrix -> Set Point
considerPoints matrix =
    matrix
        |> Set.toList
        |> List.concatMap (\point -> point::(surroundingPoints point))
        |> Set.fromList

tickPoint : Matrix -> Point -> Matrix -> Matrix
tickPoint matrix point newMatrix =
    let n = numSurrounding matrix point
        val = Set.member point matrix
    in if (not val) && n == 3 then Set.insert point newMatrix
       else if val && n >= 2 && n <= 3 then Set.insert point newMatrix
       else newMatrix

surroundingPoints : Point -> List Point
surroundingPoints (x, y) =
    -- Could I generate these?  Sure!  Where's the fun in that, though?
    [ (x - 1, y - 1)
    , (x - 1, y)
    , (x - 1, y + 1)
    , (x,     y - 1)
    , (x,     y + 1)
    , (x + 1, y - 1)
    , (x + 1, y)
    , (x + 1, y + 1)
    ]

numSurrounding : Matrix -> Point -> Int
numSurrounding matrix point =
    point
        |> surroundingPoints
        |> List.filter (at matrix)
        |> List.length

at : Matrix -> Point -> Bool
at matrix point =
    Set.member point matrix

toggle : Matrix -> Point -> Matrix
toggle matrix point =
    if Set.member point matrix then
        Set.remove point matrix
    else
        Set.insert point matrix
