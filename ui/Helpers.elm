module Helpers exposing (..)

import Bindings exposing (LibraryAlbum, MpdError(..))
import Heroicons.Outline as Icons
import Html exposing (Html, a, div, nav, object, text)
import Html.Attributes exposing (attribute, class, href, style)


grey : Int -> String
grey lightness =
    "oklch(" ++ String.fromInt lightness ++ "% 0.02 91)"


borderStyle : String
borderStyle =
    "0.125rem solid " ++ grey 80


borderClass =
    "border-b-2 border-grey-800"


containerStyle : List (Html.Attribute msg)
containerStyle =
    [ class "p-2 rounded-2xl depth"
    ]


listContainerStyle : List (Html.Attribute msg)
listContainerStyle =
    [ class "mt-2 depth rounded-3xl overflow-hidden"
    ]


listItemStyle : List (Html.Attribute msg)
listItemStyle =
    [ class "hover:bg-grey-900 px-4 py-2 border-double border-grey-800 border-b-2 last:border-b-none"
    ]


globalStyles =
    [ style "height" "inherit"
    , class "flex flex-col overflow-hidden"
    ]


roundButtonStyle =
    [ class "hover:bg-grey-900 rounded-full p-2 aspect-square overflow-hidden cursor-pointer depth"
    ]


navItems =
    [ ( "/", "Player" )
    , ( "/library/index.html", "Library" )
    ]


navBar : Html msg
navBar =
    nav
        [ class "flex justify-center"
        , class borderClass
        ]
        (List.map
            (\( path, name ) ->
                a
                    [ href path
                    , class "p-2 hover:bg-grey-950"
                    ]
                    [ text name ]
            )
            navItems
        )


albumArt : Maybe String -> String -> Html msg
albumArt albumId size =
    let
        coverArt =
            albumId
                |> Maybe.map (\id -> albumImage id 384)
                |> Maybe.withDefault "todo"
    in
    -- We use an object to provide a fallback if the image failed to load
    object
        (containerStyle
            ++ [ attribute "data" coverArt
               , attribute "loading" "lazy"
               , class size
               , class "aspect-square block p-0 overflow-hidden"
               ]
        )
        [ div [ style "background-color" (grey 95), style "width" "100%", style "height" "100%", style "padding" "20%" ]
            [ Icons.musicalNote [ style "color" (grey 50) ] ]
        ]


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
