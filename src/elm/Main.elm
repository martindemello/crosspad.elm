module Main exposing (..)

import Char exposing (..)
import Html.App as App
import Keyboard exposing (..)
import Keycode exposing (..)

import Model exposing (..)
import View exposing (..)

-- INIT --

init : (Model, Cmd Msg)
init = (Model.init {rows = 15, cols = 15}, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Keyboard.presses (\code -> KeyPress (fromCode code))
    , Keyboard.downs (\code -> KeyDown (fromKeyCode code))
    ]


-- MAIN --
main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
