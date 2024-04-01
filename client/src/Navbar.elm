module Navbar exposing (navbar)

import Model exposing (Model, Msg(..))
import Html.Attributes exposing (class, style)
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)


navbarHeight : String
navbarHeight = "100px"

addMore : Html Msg
addMore =
    div [ style "display" "flex", style "align-items" "center" ] -- Align items vertically
        [ buttonStyle (AddRows 100) "+100 rows"
        , buttonStyle (AddColumns 100) "+100 columns"
        ]


buttonStyle : Msg -> String -> Html Msg
buttonStyle msg label =
    let bStyle =
            [ style "flex" "1"
            , style "margin" "5px"
            , style "height" "30px"
            , style "width" "120px"
            , style "background-color" "#AD88C6"
            , style "color" "white"
            , style "border" "none"
            , style "cursor" "pointer"
            , style "border-radius" "5px"
            , style "overflow" "hidden" -- Ensure text doesn't span multiple rows
            , style "text-overflow" "ellipsis" -- Add ellipsis for overflow text
            , style "white-space" "nowrap" -- Prevent text wrapping
            ]
    in
    Html.button ((onClick msg) :: bStyle) [ text label ]


cellEditor : Html Msg
cellEditor =
    div [ style "flex" "3", style "margin" "5px" ]
        [ Html.input
            [ style "width" "85%"
            , style "height" "30px"
            , style "padding" "5px"
            , style "box-sizing" "border-box"
            , style "border" "none"
            , style "border-radius" "5px"
            ]
            []
        ]


navbar : Model -> Html Msg
navbar model =
    div
        [ style "position" "fixed"
        , style "height" navbarHeight
        , style "width" "100%"
        , style "top" "0px"
        , style "background-image" "linear-gradient(to bottom, #7469B6, #AD88C6)"
        , style "color" "white"
        ]

        [
        div [style "margin-left" "10px"] 
            [ div [ style "margin" "10px", style "font-size" "25px" ] [ text "Excel, supposedly" ]
            , div [
            style "width" "100%"
            , style "display" "flex"
            , style "align-items" "center"
            ] [
            addMore
            , cellEditor
            ]
            ]
        ]
