module QuAnts exposing (Cell (..), Matrix, at, empty, tick, toggle)

import Common exposing (Point)
import Dict exposing (Dict)
import Set exposing (Set)

type Cell = White | Red | Green | Blue
-- NOTE: White (empty) cells are not tracked because there are infinitely many
-- Ergo, this type is a bit misleading
type alias Matrix = Dict Point Cell

empty : Matrix
empty = Dict.empty

tick : Matrix -> Matrix
tick matrix =
    let pointsToConsider = considerPoints matrix
    in Set.foldl (tickPoint matrix) Dict.empty pointsToConsider

considerPoints : Matrix -> Set Point
considerPoints matrix =
    matrix
        |> Dict.keys
        |> List.concatMap (\point -> point::(surroundingPoints point))
        |> Set.fromList

tickPoint : Matrix -> Point -> Matrix -> Matrix
tickPoint oldMatrix point newMatrix =
    let newValue = case (marked oldMatrix point, at oldMatrix point) of
                       (True, White) -> Red
                       (True, Red) -> Blue
                       (True, Blue) -> Green
                       (True, Green) -> White
                       (False, Red) -> Green
                       (False, Green) -> Red
                       -- This is not explicitly documented, just guessing
                       (False, cell) -> cell
    in set newMatrix point newValue

marked : Matrix -> Point -> Bool
marked matrix point =
    let numClosestOdd = List.length (List.filter odd (List.map (at matrix) (closest point)))
        allEvenDiagonals = List.all even (List.map (at matrix) (diagonals point))
    in (numClosestOdd == 1 || numClosestOdd == 2) && allEvenDiagonals

odd : Cell -> Bool
odd cell =
    case cell of
        Red -> True
        Blue -> True
        _ -> False

even : Cell -> Bool
even cell = not (odd cell)

closest : Point -> List Point
closest (x, y) =
    [ (x - 1, y)
    , (x,     y - 1)
    , (x,     y + 1)
    , (x + 1, y)
    ]

diagonals : Point -> List Point
diagonals (x, y) =
    [ (x - 1, y - 1)
    , (x - 1, y + 1)
    , (x + 1, y - 1)
    , (x + 1, y + 1)
    ]

surroundingPoints : Point -> List Point
surroundingPoints point =
    List.append (closest point) (diagonals point)

at : Matrix -> Point -> Cell
at matrix point =
    Maybe.withDefault White (Dict.get point matrix)

set : Matrix -> Point -> Cell -> Matrix
set matrix point value =
    if value == White
    then Dict.remove point matrix
    else Dict.insert point value matrix

toggle : Matrix -> Point -> Matrix
toggle matrix point =
    let newValue =
            case at matrix point of
                White -> Red
                Red -> Green
                Green -> Blue
                Blue -> White
    in set matrix point newValue
