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

cellStyle : Model -> Cell -> List (Html.Attribute msg)
cellStyle model cell =
    let
        -- Define colors
        borderColorSelected = "#E1AFD1"
        borderColorClipboard = "#E1A"
        borderColorDefault = "#ccc"
        backgroundColorSelected = "#FFE6E6"
        textColor = "#000"

        baseCellStyle =
            [ Html.Attributes.style "padding" "8px"
            , Html.Attributes.style "overflow" "hidden" -- Overflow hidden within cell
            , Html.Attributes.style "white-space" "nowrap" -- Keep text in one line
            , Html.Attributes.style "text-overflow" "ellipsis" -- Show ellipsis if overflow
            , Html.Attributes.style "background-color" "white" -- Background color
            , Html.Attributes.style "user-select" "none"            
            ]

        defaultBorer = [ Html.Attributes.style "border-top" ("1px solid " ++ borderColorDefault)
                , Html.Attributes.style "border-bottom" ("1px solid " ++ borderColorDefault)
                , Html.Attributes.style "border-left" ("1px solid " ++ borderColorDefault)
                , Html.Attributes.style "border-right" ("1px solid " ++ borderColorDefault)
                ]

        selectedStyle =
            let
                (startCoord, endCoord) = model.selectedRange
                isSelected = isBetween startCoord endCoord cell.pos_x cell.pos_y 
                (startX, startY) = (startCoord.x, startCoord.y)
                (endX, endY) = (endCoord.x, endCoord.y)
                isTopEdge = cell.pos_y == min startY endY
                isBottomEdge = cell.pos_y == max startY endY
                isLeftEdge = cell.pos_x == min startX endX
                isRightEdge = cell.pos_x == max startX endX
            in
            if isSelected then
                [
                    Html.Attributes.style "background" backgroundColorSelected
                , Html.Attributes.style "border-top" (if isTopEdge then "3px solid " ++ borderColorSelected else "1px solid " ++ borderColorDefault)
                , Html.Attributes.style "border-bottom" (if isBottomEdge then "3px solid " ++ borderColorSelected else "1px solid " ++ borderColorDefault)
                , Html.Attributes.style "border-left" (if isLeftEdge then "3px solid " ++ borderColorSelected else "1px solid " ++ borderColorDefault)
                , Html.Attributes.style "border-right" (if isRightEdge then "3px solid " ++ borderColorSelected else "1px solid " ++ borderColorDefault)
                ]
            else
                defaultBorer

        clipboardStyle = 
            case model.clipboard of
                Nothing -> []
                Just (startCoord, endCoord) ->
                    let
                        isInClipboard = isBetween startCoord endCoord cell.pos_x cell.pos_y 
                        (startX, startY) = (startCoord.x, startCoord.y)
                        (endX, endY) = (endCoord.x, endCoord.y)
                        isTopEdge = cell.pos_y == min startY endY
                        isBottomEdge = cell.pos_y == max startY endY
                        isLeftEdge = cell.pos_x == min startX endX
                        isRightEdge = cell.pos_x == max startX endX
                        isOnOuterEdge = isTopEdge || isBottomEdge || isLeftEdge || isRightEdge
                    in
                        if isInClipboard && isOnOuterEdge then
                            [
                                Html.Attributes.style "border-top" (if isTopEdge then "3px dashed " ++ borderColorClipboard else "1px solid " ++ borderColorDefault)
                                , Html.Attributes.style "border-bottom" (if isBottomEdge then "3px dashed " ++ borderColorClipboard else "1px solid " ++ borderColorDefault)
                                , Html.Attributes.style "border-left" (if isLeftEdge then "3px dashed " ++ borderColorClipboard else "1px solid " ++ borderColorDefault)
                                , Html.Attributes.style "border-right" (if isRightEdge then "3px dashed " ++ borderColorClipboard else "1px solid " ++ borderColorDefault)
                            ]
                        else
                            if isInClipboard then
                                defaultBorer
                            else
                                []
    in
    baseCellStyle ++ selectedStyle ++ clipboardStyle
