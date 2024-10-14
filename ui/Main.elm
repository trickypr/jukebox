port module Main exposing (main)

import Bindings exposing (..)
import Browser
import Const exposing (..)
import Helpers exposing (albumArt, globalStyles, listContainerStyle, listItemStyle, navBar, roundButtonStyle)
import Heroicons.Outline as Icons
import Html exposing (Attribute, button, div, h2, li, text, ul)
import Html.Attributes exposing (attribute, class, style)
import Html.Events exposing (onClick)
import Http exposing (Error)
import Maybe exposing (withDefault)
import Task


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading ( Nothing, Nothing ), Cmd.batch [ getStatus, getQueue ] )



-- ports


port serverUpdate : (String -> msg) -> Sub msg



-- model


type Model
    = Loading ( Maybe MpdStatus, Maybe Queue )
    | Error MpdError
    | Status ( MpdStatus, Queue )


type Msg
    = StatusUpdate (Result Http.Error StatusResponse)
    | StatusUpdateNewQueue (Result Http.Error StatusResponse)
    | QueueUpdate (Result Http.Error QueueResponse)
    | UpdateStatus
    | UpdateQueue
    | TryLoad
    | TogglePlaying
    | Next
    | Prev
    | Noop


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

        UpdateStatus ->
            ( model, getStatus )

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

        Noop ->
            ( model, Cmd.none )



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    serverUpdate
        (\trigger ->
            case trigger of
                "queue" ->
                    UpdateQueue

                "status" ->
                    UpdateStatus

                _ ->
                    Noop
        )



-- view


view : Model -> Html.Html Msg
view model =
    div globalStyles
        [ navBar
        , case model of
            Loading _ ->
                text "Loading..."

            Error err ->
                text (Helpers.errorText err)

            Status ( status, Queue queue ) ->
                let
                    currentSongId =
                        Maybe.map (\song -> song.queueId) status.currentSong
                in
                div
                    [ class "md:flex flex-col grow gap-4 justify-center items-center p-2 overflow-scroll md:flex-row md:overflow-hidden"
                    ]
                    [ viewCurrentSong
                        status
                    , div
                        [ style "min-width" "16rem", style "height" "70%", style "display" "flex", style "flex-direction" "column" ]
                        [ h2 [ style "margin" "0" ] [ text "Queue" ]
                        , case queue of
                            [] ->
                                text "empty"

                            _ ->
                                ul (listContainerStyle ++ [ style "overflow-y" "scroll", style "height" "100%" ])
                                    (List.map (queueItem currentSongId) queue)
                        ]
                    ]
        ]


viewCurrentSong : MpdStatus -> Html.Html Msg
viewCurrentSong status =
    let
        get b a =
            List.head (List.filter b a)

        currentSongId =
            status.currentSong
                |> Maybe.andThen
                    (\current -> get (\( tag, _ ) -> tag == "MUSICBRAINZ_ALBUMID") current.song.tags)
                |> Maybe.map (\( _, releaseId ) -> releaseId)

        isDisabled =
            if status.state == Play then
                []

            else
                [ attribute "disabled" "" ]
    in
    div []
        [ albumArt currentSongId "w-full md:w-64"
        , div [ class "mt-2" ] []
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
        , div [ style "display" "flex", style "justify-content" "center", style "align-items" "center", style "gap" "0.5rem" ]
            [ button (roundButtonStyle ++ [ onClick Prev ] ++ isDisabled) [ Icons.chevronLeft [ style "width" "1.5rem", style "height" "1.5rem" ] ]
            , button (roundButtonStyle ++ [ onClick TogglePlaying ]) [ playingIcon status.state [ style "width" "2rem", style "height" "2rem" ] ]
            , button (roundButtonStyle ++ [ onClick Next ] ++ isDisabled) [ Icons.chevronRight [ style "width" "1.5rem", style "height" "1.5rem" ] ]
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


queueItem : Maybe Int -> Song -> Html.Html msg
queueItem activeQueueId song =
    li listItemStyle
        [ if song.queueId == activeQueueId then
            text "> "

          else
            text ""
        , text (withDefault "Unknown song" song.title)
        ]



-- http


getStatus : Cmd Msg
getStatus =
    Http.get { url = apiServer ++ "/api/status", expect = Http.expectJson StatusUpdate statusResponseDecoder }


getQueue : Cmd Msg
getQueue =
    Http.get { url = apiServer ++ "/api/queue", expect = Http.expectJson QueueUpdate Bindings.queueResponseDecoder }


stateChange : String -> Cmd Msg
stateChange query =
    Http.post
        { body = Http.emptyBody
        , url = apiServer ++ "/api" ++ query
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
