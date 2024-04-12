module Table exposing (..)
import Model exposing (Model)
import Html
import Html.Attributes
import Html.Events exposing (onMouseDown, onMouseUp, onMouseOver)
import Borders exposing (..)
import Array as A
import Cell exposing (..)
import Model exposing (Msg(..))
import Html.Events exposing (onDoubleClick)
import Html.Events exposing (onInput)
import Html.Attributes exposing (value)

numericalTags : Int -> List String
numericalTags n = List.map String.fromInt (List.range 0 (n - 1))
    
--List.map (\i -> convertToBase26 (i + 1)) (List.range 0 (n - 1))

-- convertToBase26 : Int -> String
-- convertToBase26 n =
--     if n <= 0 then
--         ""
--     else
--         let
--             remainder = Basics.modBy 26 (n - 1) 
--             quotient = (n - 1) // 26
--         in
--         convertToBase26 quotient ++ String.fromChar (Char.fromCode (65 + remainder))

rowToHtmlWithIndex : Int -> Model -> Html.Html Msg
rowToHtmlWithIndex rIndex model =
    let 
        sideTag = 
            Html.td 
            (  onMouseDown (PressBotBorder rIndex)
            :: onMouseOver (HoverOverBotBorder rIndex model.clickPressed)
            :: onMouseUp ReleaseMouse
            :: (sideBorderStyle model rIndex))
            [ Html.text (String.fromInt (rIndex))]
        
        cells = List.indexedMap (\cIndex _ -> 
            let 
                cellValue = Maybe.andThen (\row -> A.get rIndex row) (A.get cIndex model.values)
                cellContent = Maybe.withDefault "" (Maybe.map (\x -> case x.value of 
                    Err err -> err 
                    Ok str -> str) cellValue)
                cellStyleAttributes = Maybe.map (\cell -> cellStyle model cell) cellValue |> Maybe.withDefault []
            in
            let
                notSelected =
                    Html.td (                                                                  -- add events to cells
                       (onMouseDown (PressCell { x = cIndex, y = rIndex}))                     -- select begin
                    :: onMouseUp ReleaseMouse                                                  -- select end
                    :: onDoubleClick (EditModeCell { x = cIndex, y = rIndex})                  -- enter edit mode
                    :: (onMouseOver (HoverOver { x = cIndex, y = rIndex} model.clickPressed))  -- drag/hover, depends on clickPressed
                    :: cellStyleAttributes                                                     -- to the base style
                    ) 
                    [Html.text cellContent]
                    
                selected =
                    Html.input
                    [ value (case A.get cIndex model.values of
                        Nothing -> ""
                        Just arr -> 
                            case A.get rIndex arr of
                                Nothing -> ""
                                Just cell -> cell.content)
                    , onInput (EditCellUpdate cIndex rIndex)
                    , Html.Attributes.style "width" "100%"
                    , Html.Attributes.style "height" "30px"
                    , Html.Attributes.style "border" "3px solid #E1AFD1"
                    ] []
            in
                case model.editingCell of
                    Nothing      -> notSelected
                    Just {x, y}  -> if x == cIndex && y == rIndex then selected else notSelected
            ) (List.range 0 (model.max_y - 1))
    in
        Html.tr [] (sideTag :: cells)

createHeader : Model -> List (Html.Html Msg)
createHeader model =
    let
        headers =
            numericalTags model.max_x
        corner = Html.th [
          Html.Attributes.style "border" "1px solid black",
          Html.Attributes.style "background" "#d4d4d4"
          ] []
    in
    corner :: (List.indexedMap (\index header -> 
        Html.th (
           onMouseDown (PressTopBorder index)
        :: onMouseOver (HoverOverTopBorder index model.clickPressed)
        :: onMouseUp ReleaseMouse
        :: (headBorderStyle model index)) [Html.text header]) headers)

myTable : Model -> Html.Html Msg
myTable model =
    Html.table [
       Html.Attributes.style "border-collapse" "collapse", 
       Html.Attributes.style "width" "100%",
       Html.Attributes.style "margin-top" "100px"
    ]
        [ Html.thead [ 
          Html.Attributes.style "border-collapse" "collapse", 
          Html.Attributes.style "width" "100%"
        ]
        (createHeader model)
        , Html.tbody [
          Html.Attributes.style "text-align" "center"
        ] 
        (List.indexedMap (\index _ -> rowToHtmlWithIndex index model) (List.range 0 (model.max_y - 1)))
        ]