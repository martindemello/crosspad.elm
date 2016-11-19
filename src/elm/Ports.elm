port module Ports exposing (..)

import Types exposing (..)


port fileSelected : String -> Cmd msg


port fileContentRead : (FileReaderPortData -> msg) -> Sub msg
