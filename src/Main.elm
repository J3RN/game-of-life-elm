module Main exposing (main)

import Array exposing (Array)
import Browser exposing (element)
import Common exposing (Point)
import QuAnts exposing (Matrix, tick)
import Html exposing (Html, br, button, div, h1, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Maybe exposing (Maybe)
import Renderer exposing (cells)
import Time exposing (Posix)

main = Browser.element
       { init = init
       , update = update
       , subscriptions = subscriptions
       , view = view
       }

-- Init

type alias Model = { bounds : (Point, Point)
                   , matrix : Matrix
                   , paused : Bool
                   }

init : () -> (Model, Cmd Msg)
init _ = ( { bounds = ((0, 0), (100, 100))
           , matrix = QuAnts.empty
           , paused = True
           }
         , Cmd.none)

-- Update

type Msg = Tick Posix
         | Toggle Point
         | Pause
         | UnPause
         | Clear

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Toggle point ->
            ( { model | matrix = QuAnts.toggle model.matrix point }
            , Cmd.none
            )

        Tick _ ->
            if model.paused
            then ( model, Cmd.none )
            else ( { model | matrix = tick model.matrix }, Cmd.none )

        Pause ->
            ( { model | paused = True }, Cmd.none )

        UnPause ->
            ( { model | paused = False }, Cmd.none )

        Clear ->
            ( { model | matrix = QuAnts.empty }, Cmd.none )

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ = Time.every 35 Tick

-- View

view : Model -> Html Msg
view model =
    div [ id "main" ] [
         div [ class "controls" ]
             [ playPause model.paused
             , button [ onClick Clear ] [ text "Clear" ]
             ]
           , cells model.bounds model.matrix Toggle
           ]

playPause : Bool -> Html Msg
playPause paused =
    if paused
    then button [ onClick UnPause ] [ text "▶" ]
    else button [ onClick Pause ] [ text "⏸" ]
