module Renderer exposing (cells)


import Array exposing (Array)

import Common exposing (Point)
import Conway exposing (Matrix)
import Html exposing (Html)

import Set

import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)

type alias Msg msg = (Point -> msg)

cellSize = 10

cells : ((Int, Int), (Int, Int)) -> Matrix -> Msg msg -> Html msg
cells ((x1, y1), (x2, y2)) matrix msg =
    let xRange = List.range x1 x2
        yRange = List.range y1 y2
        heightStr = String.fromInt ((y2 - y1) * cellSize)
        widthStr = String.fromInt ((x2 - x1) * cellSize)
        rects = List.concatMap (\x -> List.map (\y -> mkCell msg x y matrix) yRange) xRange
    in svg [ style "border: 1px solid black"
           , width widthStr
           , height heightStr
           , viewBox ("0 0 " ++ widthStr ++ " " ++ heightStr)]
           rects


mkCell : Msg msg -> Int -> Int -> Matrix -> Svg msg
mkCell msg rowNum colNum matrix =
    let fillColor = if Set.member (rowNum, colNum) matrix then "black" else "white"
    in rect [ fill fillColor
            , x (String.fromInt (rowNum * cellSize))
            , y (String.fromInt (colNum * cellSize))
            , width (String.fromInt cellSize)
            , height (String.fromInt cellSize)
            , onClick (msg (rowNum, colNum))
            ] []
