module Library exposing (main)

import Bindings exposing (..)
import Browser
import Const exposing (..)
import Helpers as Helper
import Heroicons.Outline as Icons
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick, onMouseDown)
import Http
import Maybe exposing (withDefault)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias LibraryLoading =
    { songs : List Song, albums : List LibraryAlbum }


type Model
    = Loading
    | Error MpdError
    | Lib Library LibraryLoading (Maybe LibraryAlbum)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getLibrary )


type Msg
    = AddedSong String
    | AddSongToQueue Song
    | AddedAlbum String
    | AddAlbumToQueue LibraryAlbum
    | LoadLibrary (Result Http.Error (ConnectionResultWrapper Library))
    | OpenAlbum LibraryAlbum
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Lib lib loading openAlbum ->
            updateLib msg lib loading openAlbum

        _ ->
            case msg of
                LoadLibrary lib ->
                    ( lib
                        |> Helper.either (\_ -> Err MpdErrorApi) identity
                        |> Helper.either Error (\fullLib -> Lib fullLib (LibraryLoading [] []) Nothing)
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


updateLib : Msg -> Library -> LibraryLoading -> Maybe LibraryAlbum -> ( Model, Cmd Msg )
updateLib msg lib loading openAlbum =
    let
        model =
            Lib lib loading openAlbum

        { songs, albums } =
            loading
    in
    case msg of
        AddedSong songFile ->
            ( Lib lib (LibraryLoading (List.filter (\song -> song.file /= songFile) songs) albums) openAlbum, Cmd.none )

        AddedAlbum albumName ->
            ( Lib lib (LibraryLoading songs (List.filter (\album -> album.name /= albumName) albums)) openAlbum, Cmd.none )

        AddSongToQueue song ->
            ( Lib lib (LibraryLoading (song :: songs) albums) openAlbum, queueSong song.file )

        AddAlbumToQueue album ->
            ( Lib lib (LibraryLoading songs (album :: albums)) openAlbum, queueAlbum album.name )

        OpenAlbum album ->
            ( Lib lib loading (Just album), Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div Helper.globalStyles
        [ Helper.navBar
        , case model of
            Loading ->
                text "Loading..."

            Error err ->
                text (Helper.errorText err)

            Lib lib loading openAlbum ->
                div [ style "display" "flex", style "height" "100%", style "overflow" "hidden" ]
                    (viewLibrary lib loading
                        :: Maybe.withDefault [] (Maybe.map (\album -> [ albumView album ]) openAlbum)
                    )
        ]


viewLibrary : Library -> LibraryLoading -> Html Msg
viewLibrary lib loading =
    div
        [ class "grid gap-4 p-2 overflow-scroll grow"
        , style "grid-template-columns" "repeat(auto-fill, minmax(12rem, 1fr))"
        ]
        (List.map (albumCover loading) lib.albums)


albumCover : LibraryLoading -> LibraryAlbum -> Html Msg
albumCover { albums } album =
    let
        buttonContent =
            if albums |> List.map (\a -> a.name) |> List.any ((==) album.name) then
                text "Adding..."

            else
                Icons.plus [ style "width" "1rem" ]
    in
    div [ onMouseDown (OpenAlbum album), class "flex flex-col w-full pointer" ]
        [ Helper.albumArt album.musicbrainzAlbumid "12rem"
        , div [ class "flex w-full" ]
            [ div [ class "grow shrink overflow-hidden mt-0.5" ]
                [ div [ style "white-space" "nowrap", style "overflow" "hidden", style "text-overflow" "ellipsis", style "width" "100%" ] [ text album.name ]
                , div [] [ text (withDefault "Unknown artist" album.artist) ]
                ]
            , button [ onClick (AddAlbumToQueue album) ] [ buttonContent ]
            ]
        ]


albumView : LibraryAlbum -> Html Msg
albumView album =
    div
        [ style "border-left" Helper.borderStyle
        , style "height" "100%"
        , style "padding" "1rem"
        , style "overflow-y" "scroll"
        , style "min-width" "24rem"
        , class "hidden-sm"
        ]
        [ div [ style "display" "flex", style "gap" "0.5rem" ]
            [ Helper.albumArt album.musicbrainzAlbumid "12rem"
            , div []
                [ div [] [ text album.name ]
                , div [] [ text (withDefault "Unknown artist" album.artist) ]
                ]
            ]
        , ul Helper.listContainerStyle
            (List.map
                (\song ->
                    li (Helper.listItemStyle ++ [ style "cursor" "pointer", onClick (AddSongToQueue song) ])
                        [ div [] [ text (withDefault "Untitled song" song.title) ]
                        , div [] [ text (withDefault "Unknown artist" song.artist) ]
                        ]
                )
                album.songs
            )
        ]


getLibrary : Cmd Msg
getLibrary =
    Http.get
        { url = apiServer ++ "/api/library"
        , expect = Http.expectJson LoadLibrary Bindings.libraryResponseDecoder
        }


stateChange : String -> String -> (String -> Msg) -> Cmd Msg
stateChange query body msg =
    let
        handleResponse : Result Http.Error QueueAddResponse -> Msg
        handleResponse =
            Helper.either (\_ -> Noop) (Helper.either (\_ -> Noop) (\(QueueAdd str) -> msg str))
    in
    Http.post
        { body = Http.stringBody "application/text" body
        , url = apiServer ++ query
        , expect =
            Http.expectJson handleResponse queueAddResponseDecoder
        }


queueAlbum : String -> Cmd Msg
queueAlbum album =
    stateChange "/api/queue/album" album AddedAlbum


queueSong : String -> Cmd Msg
queueSong song =
    stateChange "/api/queue/song" song AddedSong
