module Cell exposing (..)
import Model exposing (..)
import Html.Attributes
import Html

isBetween : Coord -> Coord -> Int -> Int -> Bool
isBetween startCoord endCoord cx cy =
    let
        minX = min startCoord.x endCoord.x
        minY = min startCoord.y endCoord.y
        maxX = max startCoord.x endCoord.x
        maxY = max startCoord.y endCoord.y
    in
    cx >= minX && cx <= maxX && cy >= minY && cy <= maxY

cellStyle : Model -> Cell -> (Coord, Coord) -> List (Html.Attribute msg)
cellStyle _ cell (startCoord, endCoord) =
    let
        -- Define colors
        borderColorSelected = "#E1AFD1"
        borderColorDefault = "#ccc"
        backgroundColorSelected = "#FFE6E6"

        baseBorderStyle =
            [
                Html.Attributes.style "padding" "8px"
            ]

        (startX, startY) = (startCoord.x, startCoord.y)
        (endX, endY) = (endCoord.x, endCoord.y)

        isTopEdge = cell.pos_y == min startY endY
        isBottomEdge = cell.pos_y == max startY endY
        isLeftEdge = cell.pos_x == min startX endX
        isRightEdge = cell.pos_x == max startX endX

        isSelected = isBetween startCoord endCoord cell.pos_x cell.pos_y

        selectedStyle =
            if isSelected then
                [ Html.Attributes.style "background" backgroundColorSelected
                , Html.Attributes.style "border-top" (if isTopEdge then "3px solid " ++ borderColorSelected else "1px solid " ++ borderColorDefault)
                , Html.Attributes.style "border-bottom" (if isBottomEdge then "3px solid " ++ borderColorSelected else "1px solid " ++ borderColorDefault)
                , Html.Attributes.style "border-left" (if isLeftEdge then "3px solid " ++ borderColorSelected else "1px solid " ++ borderColorDefault)
                , Html.Attributes.style "border-right" (if isRightEdge then "3px solid " ++ borderColorSelected else "1px solid " ++ borderColorDefault)
                ]
            else
                [
                  Html.Attributes.style "border-top" ("1px solid " ++ borderColorDefault)
                , Html.Attributes.style "border-bottom" ("1px solid " ++ borderColorDefault)
                , Html.Attributes.style "border-left" ("1px solid " ++ borderColorDefault)
                , Html.Attributes.style "border-right" ("1px solid " ++ borderColorDefault)
                ]
    in
    baseBorderStyle ++ selectedStyle