module Types exposing (..)

import Json.Decode as Json exposing (..)
import Keycode exposing (Key)


type Symmetry
    = SymmNone
    | Symm180
    | Symm90
    | GridLocked


type Direction
    = Across
    | Down


type Msg
    = ClickSquare Int Int
    | KeyPress Char
    | KeyDown Key
    | GridLostFocus
    | GridGainedFocus
    | SetSymmetry Symmetry
    | ToggleDirection
    | UploadFile
    | FileUploaded FileReaderPortData
    | SaveFile
    | FileConverted String
    | LoadFormatChanged String
    | SaveFormatChanged String
    | SetCurrentClue Direction Int


type alias FileReaderPortData =
    Json.Value


type alias LoadFileRequest =
    { file_element : String, format : String }


type alias SaveFileRequest =
    { filename_element : String
    , data : String
    , format : String
    }
