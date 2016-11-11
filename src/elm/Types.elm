module Types exposing (..)

import Json.Decode as Json exposing (..)
import Keycode exposing (Key)

type Symmetry = SymmNone | Symm180 | Symm90 | GridLocked

type Direction = Across | Down

type alias FileReaderPortData = Json.Value

type Msg =
  ClickSquare Int Int
  | KeyPress Char
  | KeyDown Key
  | SetSymmetry Symmetry
  | ToggleDirection
  | UploadFile
  | FileUploaded FileReaderPortData
  | SaveFile

