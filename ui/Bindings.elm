
-- generated by elm_rs


module Bindings exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode
import Json.Encode
import Url.Builder


resultEncoder : (e -> Json.Encode.Value) -> (t -> Json.Encode.Value) -> (Result e t -> Json.Encode.Value)
resultEncoder errEncoder okEncoder enum =
    case enum of
        Ok inner ->
            Json.Encode.object [ ( "Ok", okEncoder inner ) ]
        Err inner ->
            Json.Encode.object [ ( "Err", errEncoder inner ) ]


resultDecoder : Json.Decode.Decoder e -> Json.Decode.Decoder t -> Json.Decode.Decoder (Result e t)
resultDecoder errDecoder okDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map Ok (Json.Decode.field "Ok" okDecoder)
        , Json.Decode.map Err (Json.Decode.field "Err" errDecoder)
        ]


type MpdError
    = MpdErrorApi
    | MpdErrorConnection
    | MpdErrorCommunication


mpdErrorDecoder : Json.Decode.Decoder MpdError
mpdErrorDecoder = 
    Json.Decode.oneOf
        [ Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "MpdErrorApi" ->
                            Json.Decode.succeed MpdErrorApi
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        , Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "MpdErrorConnection" ->
                            Json.Decode.succeed MpdErrorConnection
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        , Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "MpdErrorCommunication" ->
                            Json.Decode.succeed MpdErrorCommunication
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        ]

type MpdState
    = Play
    | Pause
    | Stop


mpdStateDecoder : Json.Decode.Decoder MpdState
mpdStateDecoder = 
    Json.Decode.oneOf
        [ Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "Play" ->
                            Json.Decode.succeed Play
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        , Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "Pause" ->
                            Json.Decode.succeed Pause
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        , Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "Stop" ->
                            Json.Decode.succeed Stop
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        ]

type alias Song =
    { file : String
    , name : Maybe (String)
    , title : Maybe (String)
    , artist : Maybe (String)
    , duration : Maybe (Int)
    , tags : List (( String, String ))
    , queueId : Maybe (Int)
    }


songDecoder : Json.Decode.Decoder Song
songDecoder =
    Json.Decode.succeed Song
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "file" (Json.Decode.string)))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "name" (Json.Decode.nullable (Json.Decode.string))))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "title" (Json.Decode.nullable (Json.Decode.string))))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "artist" (Json.Decode.nullable (Json.Decode.string))))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "duration" (Json.Decode.nullable (Json.Decode.int))))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "tags" (Json.Decode.list (Json.Decode.map2 (\a b -> ( a, b )) (Json.Decode.index 0 (Json.Decode.string)) (Json.Decode.index 1 (Json.Decode.string))))))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "queue_id" (Json.Decode.nullable (Json.Decode.int))))


type alias QueueSong =
    { queueId : Int
    , queuePos : Int
    , song : Song
    }


queueSongDecoder : Json.Decode.Decoder QueueSong
queueSongDecoder =
    Json.Decode.succeed QueueSong
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "queue_id" (Json.Decode.int)))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "queue_pos" (Json.Decode.int)))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "song" (songDecoder)))


type MpdSongShot
    = Default
    | Single
    | SingleRepeat
    | QueueRepeat


mpdSongShotDecoder : Json.Decode.Decoder MpdSongShot
mpdSongShotDecoder = 
    Json.Decode.oneOf
        [ Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "Default" ->
                            Json.Decode.succeed Default
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        , Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "Single" ->
                            Json.Decode.succeed Single
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        , Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "SingleRepeat" ->
                            Json.Decode.succeed SingleRepeat
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        , Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case x of
                        "QueueRepeat" ->
                            Json.Decode.succeed QueueRepeat
                        unexpected ->
                            Json.Decode.fail <| "Unexpected variant " ++ unexpected
                )
        ]

type alias MpdStatus =
    { state : MpdState
    , currentSong : Maybe (QueueSong)
    , currentElapsed : Maybe (Int)
    , currentDuration : Maybe (Int)
    , volume : Maybe (Int)
    , shot : MpdSongShot
    , random : Bool
    , consume : Bool
    }


mpdStatusDecoder : Json.Decode.Decoder MpdStatus
mpdStatusDecoder =
    Json.Decode.succeed MpdStatus
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "state" (mpdStateDecoder)))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "current_song" (Json.Decode.nullable (queueSongDecoder))))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "current_elapsed" (Json.Decode.nullable (Json.Decode.int))))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "current_duration" (Json.Decode.nullable (Json.Decode.int))))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "volume" (Json.Decode.nullable (Json.Decode.int))))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "shot" (mpdSongShotDecoder)))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "random" (Json.Decode.bool)))
        |> Json.Decode.andThen (\x -> Json.Decode.map x (Json.Decode.field "consume" (Json.Decode.bool)))


type Queue
    = Queue (List (Song))


queueDecoder : Json.Decode.Decoder Queue
queueDecoder =
    Json.Decode.map Queue (Json.Decode.list (songDecoder))



type ConnectionResultWrapper a
    = MpdError MpdError
    | MpdOk a


connectionResultWrapperDecoder : Json.Decode.Decoder a -> Json.Decode.Decoder (ConnectionResultWrapper a)
connectionResultWrapperDecoder decoder = 
    Json.Decode.oneOf
        [ Json.Decode.map MpdError (Json.Decode.field "MpdError" (mpdErrorDecoder))
        , Json.Decode.map MpdOk (Json.Decode.field "MpdOk" (decoder))
        ]


type alias StatusResponse = ConnectionResultWrapper MpdStatus

statusResponseDecoder : Json.Decode.Decoder StatusResponse
statusResponseDecoder = connectionResultWrapperDecoder mpdStatusDecoder

type alias QueueResponse = ConnectionResultWrapper Queue

queueResponseDecoder : Json.Decode.Decoder QueueResponse
queueResponseDecoder = connectionResultWrapperDecoder queueDecoder

