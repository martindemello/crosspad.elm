module Main exposing (..)

import Char exposing (..)
import Html.App as App
import Keyboard exposing (..)
import Keycode exposing (..)

import Model exposing (..)
import View exposing (..)
import Xword exposing (..)


-- INIT --

init_model : Model
init_model =
    let w = 15
        h = 15
        xw = make_xword { rows = h, cols = w }
    in
    { width = w
    , height = h
    , xw = xw
    , cur_x = 0
    , cur_y = 0
    , cur_dir = Across
    , last_key = "None"
    , symmetry = Symm180
    }

init : (Model, Cmd Msg)
init = (init_model, Cmd.none)

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
