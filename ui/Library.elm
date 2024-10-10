module Library exposing (main)

import Bindings exposing (..)
import Browser
import Debug exposing (todo)
import Helpers as Helper
import Html exposing (..)
import Html.Attributes exposing (attribute, src, style)
import Html.Events exposing (onClick)
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
    | Lib Library LibraryLoading


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getLibrary )


type Msg
    = AddedSong String
    | AddSongToQueue Song
    | AddedAlbum String
    | AddAlbumToQueue LibraryAlbum
    | LoadLibrary (Result Http.Error (ConnectionResultWrapper Library))
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Lib lib loading ->
            updateLib msg lib loading

        _ ->
            case msg of
                LoadLibrary lib ->
                    ( lib
                        |> Helper.either (\_ -> Err MpdErrorApi) identity
                        |> Helper.either Error (\fullLib -> Lib fullLib (LibraryLoading [] []))
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


updateLib : Msg -> Library -> LibraryLoading -> ( Model, Cmd Msg )
updateLib msg lib loading =
    let
        model =
            Lib lib loading

        { songs, albums } =
            loading
    in
    case msg of
        AddedSong songFile ->
            ( Lib lib (LibraryLoading (List.filter (\song -> song.file /= songFile) songs) albums), Cmd.none )

        AddedAlbum albumName ->
            ( Lib lib (LibraryLoading songs (List.filter (\album -> album.name /= albumName) albums)), Cmd.none )

        AddSongToQueue song ->
            ( Lib lib (LibraryLoading (song :: songs) albums), queueSong song.file )

        AddAlbumToQueue album ->
            ( Lib lib (LibraryLoading songs (album :: albums)), queueAlbum album.name )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            text "Loadinng..."

        Error err ->
            text (Helper.errorText err)

        Lib lib loading ->
            viewLibrary lib loading


viewLibrary : Library -> LibraryLoading -> Html Msg
viewLibrary lib loading =
    div Helper.globalStyles
        (List.map (albumCover loading) lib.albums)


albumCover : LibraryLoading -> LibraryAlbum -> Html Msg
albumCover { albums } album =
    let
        coverArt =
            album.musicbrainzAlbumid
                |> Maybe.map (\id -> Helper.albumImage id 384)
                |> Maybe.withDefault "todo"

        buttonContent =
            if albums |> List.map (\a -> a.name) |> List.any ((==) album.name) then
                "Adding..."

            else
                "Add to queue"
    in
    div []
        [ div [] [ img [ src coverArt, attribute "loading" "lazy", style "width" "12rem", style "height" "12rem" ] [] ]
        , div [] [ text album.name ]
        , div [] [ text (withDefault "Unknown artist" album.artist) ]
        , button [ onClick (AddAlbumToQueue album) ] [ text buttonContent ]
        ]


getLibrary : Cmd Msg
getLibrary =
    Http.get
        { url = "http://localhost:3000/library"
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
        , url = "http://localhost:3000" ++ query
        , expect =
            Http.expectJson handleResponse queueAddResponseDecoder
        }


queueAlbum : String -> Cmd Msg
queueAlbum album =
    stateChange "/queue/album" album AddedAlbum


queueSong : String -> Cmd Msg
queueSong song =
    stateChange "/queue/song" song AddedSong
