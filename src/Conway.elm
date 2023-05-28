module Conway exposing (Matrix, empty, tick, toggle)

import Array exposing (Array)

type alias Cell = Bool
type alias Row = Array Cell
type alias Matrix = Array Row

sideLength = 100

empty : Matrix
empty = Array.repeat sideLength (Array.repeat sideLength False)

-- This is probably wildly unperformant, and I am ashamed
tick : Matrix -> Matrix
tick matrix =
    Array.indexedMap (tickRow matrix) matrix

tickRow : Matrix -> Int -> Row -> Row
tickRow matrix rowNum row =
    Array.indexedMap (tickCell matrix rowNum) row

tickCell : Matrix -> Int -> Int -> Bool -> Bool
tickCell matrix rowNum colNum val =
    let n = numSurrounding matrix rowNum colNum
    in if (not val) && n == 3 then True
       else if val && n >= 2 && n <= 3 then val
       else False

numSurrounding : Matrix -> Int -> Int -> Int
numSurrounding matrix rowNum colNum =
    -- Could I generate these?  Sure!  Where's the fun in that, though?
    let indices = [ (rowNum - 1, colNum - 1)
                  , (rowNum - 1, colNum)
                  , (rowNum - 1, colNum + 1)
                  , (rowNum, colNum - 1)
                  , (rowNum, colNum + 1)
                  , (rowNum + 1, colNum - 1)
                  , (rowNum + 1, colNum)
                  , (rowNum + 1, colNum + 1)
                  ]
    in indices
        |> List.map (\(y, x) -> at matrix y x)
        |> List.filter (\x -> x)
        |> List.length

-- If the indicated cells is out-of-bounds, False is returned
at : Matrix -> Int -> Int -> Bool
at matrix rowNum colNum =
    let mRow = Array.get rowNum matrix
        mVal = Maybe.andThen (Array.get colNum) mRow
    in Maybe.withDefault False mVal

toggle : Int -> Int -> Matrix -> Matrix
toggle rowNum colNum matrix =
    -- This code bigtime sucks.  Applicative would make it better, but I'm sure
    -- there's other ways.
    let oldRow = Array.get rowNum matrix
        oldVal = Maybe.andThen (Array.get colNum) oldRow
        newRow = Maybe.andThen (\row -> Maybe.map (\old -> Array.set colNum (not old) row) oldVal) oldRow
        newMatrix = Maybe.map (\row -> Array.set rowNum row matrix) newRow
    in Maybe.withDefault matrix newMatrix
