module Renderer exposing (cells)

import Array exposing (Array)

import Html exposing (Html)

import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)

type alias Msg msg = (Int -> Int -> msg)

cellSize = 10

cells : Array (Array Bool) -> Msg msg -> Html msg
cells matrix msg =
    let rows = Array.toList (Array.indexedMap (mkRow msg) matrix)
        myHeight = cellSize * (Array.length matrix)
        heightStr = String.fromInt myHeight
        widthStr = Array.get 0 matrix |> Maybe.map (\row -> Array.length(row) * cellSize) |> Maybe.withDefault myHeight |> String.fromInt
    in svg [ style "border: 1px solid black"
           , width widthStr
           , height heightStr
           , viewBox ("0 0 " ++ widthStr ++ " " ++ heightStr)]
        (List.foldl List.append [] rows)

mkRow : Msg msg -> Int -> Array Bool -> List (Svg msg)
mkRow msg rowNum rowCells =
    Array.toList (Array.indexedMap (mkCell msg rowNum) rowCells)

mkCell : Msg msg -> Int -> Int -> Bool -> Svg msg
mkCell msg rowNum colNum val =
    let fillColor = if val then "black" else "white"
    in rect [ fill fillColor
            , x (String.fromInt (rowNum * cellSize))
            , y (String.fromInt (colNum * cellSize))
            , width (String.fromInt cellSize)
            , height (String.fromInt cellSize)
            , onClick (msg rowNum colNum)
            ] []
