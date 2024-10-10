module Helpers exposing (..)

import Bindings exposing (MpdError(..))
import Html.Attributes exposing (style)


globalStyles =
    [ style "font-family" "sans-serif" ]


either : (a -> c) -> (b -> c) -> Result a b -> c
either errCallback okCallback result =
    case result of
        Ok v ->
            okCallback v

        Err err ->
            errCallback err


andThenErr : (x -> Result y a) -> Result x a -> Result y a
andThenErr callback result =
    case result of
        Ok value ->
            Ok value

        Err err ->
            callback err


flatten : Result x (Result x a) -> Result x a
flatten outer =
    case outer of
        Ok inner ->
            inner

        Err err ->
            Err err


errorText : MpdError -> String
errorText err =
    case err of
        MpdErrorApi ->
            "Failed to connect to api"

        MpdErrorConnection ->
            "Failed to find a running mpd instance"

        MpdErrorCommunication ->
            "Invalid message sent from the mpd server"


albumImage : String -> Int -> String
albumImage releaseId size =
    "https://imagecdn.app/v1/images/https%3A%2F%2Fcoverartarchive.org%2Frelease%2F"
        ++ releaseId
        ++ "%2Ffront?width="
        ++ String.fromInt size
