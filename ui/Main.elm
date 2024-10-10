module Main exposing (main)

import Bindings exposing (..)
import Browser
import Helpers
import Heroicons.Outline as Icons
import Html exposing (Attribute, button, div, h2, img, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http exposing (Error)
import List exposing (map)
import Maybe exposing (withDefault)
import String exposing (fromInt)
import Task


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading ( Nothing, Nothing ), Cmd.batch [ getStatus, getQueue ] )



-- model


type Model
    = Loading ( Maybe MpdStatus, Maybe Queue )
    | Error MpdError
    | Status ( MpdStatus, Queue )


type Msg
    = StatusUpdate (Result Http.Error StatusResponse)
    | StatusUpdateNewQueue (Result Http.Error StatusResponse)
    | QueueUpdate (Result Http.Error QueueResponse)
    | UpdateQueue
    | TryLoad
    | TogglePlaying
    | Next
    | Prev


setQueue : Queue -> Model -> Model
setQueue queue model =
    case model of
        Error _ ->
            model

        Loading ( status, _ ) ->
            Loading ( status, Just queue )

        Status ( status, _ ) ->
            Status ( status, queue )


setStatus : MpdStatus -> Model -> Model
setStatus status model =
    case model of
        Error _ ->
            model

        Loading ( _, queue ) ->
            Loading ( Just status, queue )

        Status ( _, queue ) ->
            Status ( status, queue )


handleUpdate : (a -> Model -> Model) -> Model -> Result Http.Error (ConnectionResultWrapper a) -> Cmd Msg -> ( Model, Cmd Msg )
handleUpdate apply model result successTask =
    result
        |> Helpers.either (\_ -> Err MpdErrorApi) identity
        |> Helpers.either (\err -> ( Error err, Cmd.none )) (\value -> ( apply value model, successTask ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TogglePlaying ->
            ( model, togglePlaying )

        Prev ->
            ( model, playbackPrev )

        Next ->
            ( model, playbackNext )

        UpdateQueue ->
            ( model, getQueue )

        StatusUpdate result ->
            handleUpdate setStatus model result (Task.succeed TryLoad |> Task.perform identity)

        StatusUpdateNewQueue result ->
            handleUpdate setStatus model result (Task.succeed UpdateQueue |> Task.perform identity)

        QueueUpdate result ->
            handleUpdate setQueue model result (Task.succeed TryLoad |> Task.perform identity)

        TryLoad ->
            case model of
                Loading ( Just status, Just queue ) ->
                    ( Status ( status, queue ), Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- view


grey : Int -> String
grey lightness =
    "oklch(" ++ fromInt lightness ++ "% 0.02 91)"


containerStyle : List (Html.Attribute msg)
containerStyle =
    [ style "padding" "1rem", style "border-radius" "0.75rem", style "border" ("0.125rem solid " ++ grey 80), style "border-bottom-width" "0.25rem" ]


view : Model -> Html.Html Msg
view model =
    case model of
        Loading _ ->
            text "Loading..."

        Error err ->
            text (Helpers.errorText err)

        Status ( status, Queue queue ) ->
            div (Helpers.globalStyles ++ [ style "display" "flex", style "gap" "1rem", style "justify-content" "center", style "padding" "16rem" ])
                [ viewCurrentSong
                    status
                , div
                    (containerStyle
                        ++ [ style "min-width" "16rem" ]
                    )
                    [ h2 [] [ text "Queue" ]
                    , case queue of
                        [] ->
                            text "empty"

                        _ ->
                            viewQueue queue (Maybe.map (\song -> song.queueId) status.currentSong)
                    ]
                ]


viewCurrentSong : MpdStatus -> Html.Html Msg
viewCurrentSong status =
    let
        get b a =
            List.head (List.filter b a)

        currentSongId =
            Maybe.andThen
                (\current -> get (\( tag, _ ) -> tag == "MUSICBRAINZ_ALBUMID") current.song.tags)
                status.currentSong

        maybeCoverImage =
            Maybe.map (\( _, releaseId ) -> Helpers.albumImage releaseId 500) currentSongId
    in
    div []
        [ -- Albumn art placeholder
          div (containerStyle ++ [ style "width" "24rem", style "height" "24rem", style "padding" "0", style "overflow" "hidden" ])
            (Maybe.withDefault []
                (Maybe.map (\source -> [ img [ Html.Attributes.src source, style "width" "100%" ] [] ]) maybeCoverImage)
            )
        , withDefault (div [] [])
            (Maybe.map
                (\currentSong ->
                    div [ style "text-align" "center" ]
                        [ div [] [ text (withDefault "Unknown song" currentSong.song.title) ]
                        , div [] [ text (withDefault "Unknown artist" currentSong.song.artist) ]
                        ]
                )
                status.currentSong
            )
        , div []
            [ button [ onClick Prev ] [ Icons.chevronLeft [ style "width" "1.5rem", style "height" "1.5rem" ] ]
            , button [ onClick TogglePlaying ] [ playingIcon status.state [ style "width" "2rem", style "height" "2rem" ] ]
            , button [ onClick Next ] [ Icons.chevronRight [ style "width" "1.5rem", style "height" "1.5rem" ] ]
            ]
        ]


playingIcon : MpdState -> List (Attribute msg) -> Html.Html msg
playingIcon state =
    case state of
        Play ->
            Icons.pause

        Pause ->
            Icons.play

        Stop ->
            Icons.play


viewQueue : List Song -> Maybe Int -> Html.Html msg
viewQueue songs activeQueueId =
    div []
        (map
            (\song ->
                div []
                    [ if song.queueId == activeQueueId then
                        text "> "

                      else
                        text ""
                    , text (withDefault "Unknown song" song.title)
                    ]
            )
            songs
        )



-- http


getStatus : Cmd Msg
getStatus =
    Http.get { url = "http://localhost:3000/status", expect = Http.expectJson StatusUpdate statusResponseDecoder }


getQueue : Cmd Msg
getQueue =
    Http.get { url = "http://localhost:3000/queue", expect = Http.expectJson QueueUpdate Bindings.queueResponseDecoder }


stateChange : String -> Cmd Msg
stateChange query =
    Http.post
        { body = Http.emptyBody
        , url = "http://localhost:3000" ++ query
        , expect =
            Http.expectJson StatusUpdateNewQueue
                statusResponseDecoder
        }


togglePlaying : Cmd Msg
togglePlaying =
    stateChange "/playback/toggle"


playbackPrev : Cmd Msg
playbackPrev =
    stateChange "/playback/prev"


playbackNext : Cmd Msg
playbackNext =
    stateChange "/playback/next"
