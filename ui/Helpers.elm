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


containerStyle : List (Html.Attribute msg)
containerStyle =
    [ style "padding" "1rem", style "border-radius" "1rem", style "border" borderStyle, style "border-bottom-width" "0.25rem" ]


listContainerStyle : List (Html.Attribute msg)
listContainerStyle =
    [ style "overflow" "hidden"
    , style "padding" "0"
    , style "padding-bottom" "-0.125rem"
    , style "border" borderStyle
    , style "border-bottom-width" "0.25rem"
    , style "border-radius" "2rem"
    , class "last-borderless"
    ]


listItemStyle : List (Html.Attribute msg)
listItemStyle =
    [ attribute "style" ("--hover-bg:" ++ grey 90)
    , style "padding" "0.5rem 1rem"
    , style "border-bottom" borderStyle
    , style "border-bottom-style" "double"
    ]


globalStyles =
    [ style "font-family" "sans-serif"
    , style "height" "inherit"
    , style "display" "flex"
    , style "flex-direction" "column"
    , style "overflow" "hidden"
    ]


roundButtonStyle =
    [ attribute "style" ("--hover-bg:" ++ grey 90)
    , style "background-color" "white"
    , style "border" borderStyle
    , style "border-radius" "100000px"
    , style "padding" "0.25rem"
    , style "aspect-ratio" "1/1"
    , style "border-bottom-width" "0.2rem"
    , style "overflow" "hidden"
    , style "cursor" "pointer"
    ]


navItems =
    [ ( "/", "Player" )
    , ( "/library/index.html", "Library" )
    ]


navBar : Html msg
navBar =
    nav
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "border-bottom" borderStyle
        ]
        (List.map
            (\( path, name ) ->
                a
                    [ href path
                    , attribute "style" ("--hover-bg:" ++ grey 95)
                    , style "padding" "0.5rem"
                    , style "color" "black"
                    , style "text-decoration" "none"
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
               , style "width" size
               , style "height" size
               , style "display" "block"
               , style "padding" "0"
               , style "overflow" "hidden"
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
