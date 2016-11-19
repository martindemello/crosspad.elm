module Main exposing (..)

import Char exposing (..)
import Html
import Keyboard exposing (..)
import Keycode exposing (..)
import Model exposing (..)
import Ports
import Types exposing (..)
import Update exposing (..)
import View exposing (..)


-- INIT --


init : ( Model, Cmd Msg )
init =
    ( Model.init { rows = 15, cols = 15 }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.presses (\code -> KeyPress (fromCode code))
        , Keyboard.downs (\code -> KeyDown (fromKeyCode code))
        , Ports.fileContentRead FileUploaded
        , Ports.saveFileReturned FileConverted
        ]



-- MAIN --


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
