module Update exposing (..)

import Json.Decode exposing (Value)
import Model exposing (..)
import Ports exposing (..)
import Reader
import Types exposing (..)


read_file : Json.Decode.Value -> Model -> Model
read_file f model =
    let
        res =
            Reader.decode f
    in
        case res of
            Ok xw_ ->
                { model | xw = xw_, symmetry = GridLocked }

            _ ->
                model


update_model : Msg -> Model -> Model
update_model msg model =
    case msg of
        ClickSquare x y ->
            set_cursor x y model

        KeyDown k ->
            handle_keycode k model

        KeyPress c ->
            handle_keypress c model

        SetSymmetry s ->
            handle_symm s model

        ToggleDirection ->
            toggle_dir model

        UploadFile ->
            model

        FileUploaded f ->
            read_file f model

        SaveFile ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UploadFile ->
            ( model, fileSelected "file-upload" )

        _ ->
            ( update_model msg model, Cmd.none )
