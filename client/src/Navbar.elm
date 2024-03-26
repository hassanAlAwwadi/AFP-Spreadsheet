module Navbar exposing (navbar)
import Model exposing (Model, Msg(..))
import Html.Attributes
import Html exposing (..)
import Html.Events exposing (onClick)

navbarHeight : String
navbarHeight = "100px"

addMore : Html.Html Msg
addMore =
    Html.div []
    [
        Html.button [
        onClick (AddRows 100),
        Html.Attributes.style "margin-bottom" "5px",
        Html.Attributes.style "height" "30px"
    ] [Html.text "Add 100 rows"],
        Html.button [
        onClick (AddColumns 100),
        Html.Attributes.style "margin" "5px",
        Html.Attributes.style "height" "30px"
    ] [Html.text "Add 100 columns"]
    ]

navbar : Model -> Html.Html Msg
navbar model =
    Html.div
        [ Html.Attributes.style "position" "fixed"
        , Html.Attributes.style "height" navbarHeight
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "top" "0px"
        , Html.Attributes.style "background-image" "linear-gradient(to bottom, #7469B6, #AD88C6)"
        , Html.Attributes.style "color" "white"
        ]
        [ Html.div 
            [
                Html.Attributes.style "margin" "10px"
            ,   Html.Attributes.style "font-size" "25px"
            ]
            [Html.text "Excel, supposedly", 
            addMore]
        ]