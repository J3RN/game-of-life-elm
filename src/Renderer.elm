module Renderer exposing (cells)

import Array exposing (Array)

import Html exposing (Html)

import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)

type alias Msg msg = (Int -> Int -> msg)

cells : Array (Array Bool) -> Msg msg -> Html msg
cells matrix msg =
    let rows = Array.toList (Array.indexedMap (mkRow msg) matrix)
    in svg [style "border: 1px solid black", width "500", height "500", viewBox "0 0 500 500"] (List.foldl List.append [] rows)

mkRow : Msg msg -> Int -> Array Bool -> List (Svg msg)
mkRow msg rowNum rowCells =
    Array.toList (Array.indexedMap (mkCell msg rowNum) rowCells)

mkCell : Msg msg -> Int -> Int -> Bool -> Svg msg
mkCell msg rowNum colNum val =
    let fillColor = if val then "black" else "white"
    in rect [ fill fillColor
            , x (String.fromInt (rowNum * 5))
            , y (String.fromInt (colNum * 5))
            , width "5"
            , height "5"
            , onClick (msg rowNum colNum)
            ] []
