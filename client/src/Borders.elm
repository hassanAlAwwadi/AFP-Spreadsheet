module Borders exposing (..)
import Html.Attributes
import Html
import Model exposing (Model)
import Model exposing (Coord)
import Cell exposing (isBetween)

isBetween : Int -> Int -> Int -> Bool
isBetween a b x = 
    let 
        big   = max a b
        small =  min a b
    in
        small <= x && x <= big

sideBorderStyle : Model -> Int -> List (Html.Attribute msg)
sideBorderStyle model index =
    let baseStyle = 
            [ Html.Attributes.style "border" "1px solid #999"
            , Html.Attributes.style "padding" "8px"
            , Html.Attributes.style "max-width" "40px"
            , Html.Attributes.style "min-height" "20px"
            , Html.Attributes.style "height" "20px"
            , Html.Attributes.style "min-width" "40px"
            , Html.Attributes.style "position" "sticky"       -- Make it sticky
            , Html.Attributes.style "left" "0" 
            , Html.Attributes.style "top" "100px"             -- Stick 100px away from the top
            , Html.Attributes.style "user-select" "none"
            ]
        background =
            case model.selectedRange of
                (start, end) -> 
                    if isBetween start.y end.y index 
                    then [Html.Attributes.style "background" "#FFE6E6"]
                    else [Html.Attributes.style "background" "#f5f5f5"]
    in
    baseStyle ++ background

headBorderStyle  : Model -> Int -> List (Html.Attribute msg)
headBorderStyle model index =
    let baseStyle =
            [ Html.Attributes.style "border" "1px solid #999" 
            , Html.Attributes.style "padding" "8px"
            , Html.Attributes.style "background" "#f5f5f5"
            , Html.Attributes.style "min-width" "40px"
            , Html.Attributes.style "height" "15px"
            , Html.Attributes.style "position" "sticky" -- Make it sticky
            , Html.Attributes.style "top" "100px"
            , Html.Attributes.style "left" "0"          -- Stick to the left
            , Html.Attributes.style "z-index" "1"       -- Ensure the top border is on top
            , Html.Attributes.style "user-select" "none"
            ]
        background =
            case model.selectedRange of
                (start, end) -> 
                    if isBetween start.x end.x index 
                    then [Html.Attributes.style "background" "#FFE6E6"]
                    else [Html.Attributes.style "background" "#f5f5f5"]
    in
    baseStyle ++ background
    