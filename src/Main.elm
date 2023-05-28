module Main exposing (main)

import Array exposing (Array)
import Browser exposing (element)
import Conway exposing (Matrix, tick)
import Renderer exposing (cells)
import Html exposing (Html, br, button, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Maybe exposing (Maybe)
import Time exposing (Posix)

main = Browser.element
       { init = init
       , update = update
       , subscriptions = subscriptions
       , view = view
       }

-- Init

type alias Model = { matrix : Matrix
                   , paused : Bool
                   }

init : () -> (Model, Cmd Msg)
init _ = ( { matrix = Conway.empty
           , paused = True
           }
         , Cmd.none)

-- Update

type Msg = Tick Posix
         | Toggle Int Int
         | Pause
         | UnPause
         | Clear

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Toggle y x ->
            ( { model | matrix = Conway.toggle y x model.matrix }
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
            ( { model | matrix = Conway.empty }, Cmd.none )

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ = Time.every 35 Tick

-- View

view : Model -> Html Msg
view model =
    div [] [ h1 [] [ text "Conway's Game of Life" ]
           , div [ class "controls" ]
                 [ playPause model.paused
                 , button [ onClick Clear ] [ text "Clear" ] ]
           , cells model.matrix Toggle
           ]

playPause : Bool -> Html Msg
playPause paused =
    if paused
    then button [ onClick UnPause ] [ text "▶" ]
    else button [ onClick Pause ] [ text "⏸" ]
