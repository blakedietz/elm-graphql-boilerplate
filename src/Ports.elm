port module Ports exposing (..)


port dumpModel : (() -> msg) -> Sub msg


port saveSessionId : Maybe String -> Cmd msg
