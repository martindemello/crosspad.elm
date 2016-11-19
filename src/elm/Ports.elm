port module Ports exposing (..)

import Types exposing (..)


port fileSelected : LoadFileRequest -> Cmd msg


port fileContentRead : (FileReaderPortData -> msg) -> Sub msg


port saveFileRequested : SaveFileRequest -> Cmd msg


port saveFileReturned : (String -> msg) -> Sub msg
