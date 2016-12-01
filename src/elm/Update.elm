module Update exposing (..)

import Cursor
import Json.Decode exposing (Value)
import Model exposing (..)
import Ports exposing (..)
import Reader
import Types exposing (..)
import Writer


load_file_request : Model -> LoadFileRequest
load_file_request model =
    { file_element = "file-upload"
    , format = model.load_format
    }


save_file_request : Model -> SaveFileRequest
save_file_request model =
    { data = Writer.to_json_string model.xw
    , format = model.save_format
    , filename_element = "file-download"
    }


read_file : Json.Decode.Value -> Model -> Model
read_file f model =
    let
        res =
            Reader.decode f
    in
        case res of
            Ok xw_ ->
                { model
                    | xw = xw_
                    , cursor = Cursor.init xw_
                    , symmetry = GridLocked
                }
                    |> update_all

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

        GridLostFocus ->
            set_grid_focus False model

        GridGainedFocus ->
            set_grid_focus True model

        SetSymmetry s ->
            handle_symm s model

        ToggleDirection ->
            toggle_dir model

        UploadFile ->
            model

        SaveFile ->
            model

        FileUploaded f ->
            read_file f model

        FileConverted s ->
            model

        LoadFormatChanged f ->
            handle_load_format_changed f model

        SaveFormatChanged f ->
            handle_save_format_changed f model

        SetCurrentClue Across n ->
            set_current_across_clue n model

        SetCurrentClue Down n ->
            set_current_down_clue n model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UploadFile ->
            ( model, fileSelected (load_file_request model) )

        SaveFile ->
            ( model, saveFileRequested (save_file_request model) )

        _ ->
            ( update_model msg model, Cmd.none )
