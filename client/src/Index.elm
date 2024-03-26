module Index exposing (..)
import Array as A
import Browser
import Html.Attributes exposing (attribute)
import Html exposing (node)
import Model exposing (Model, Cell, Coord)
import Html.Events exposing (onClick,onMouseDown, onMouseOver, onMouseUp)
import Browser.Events

main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }

init : Model
init = 
    let
        numRows = 100
        numCols = 100
        initialGrid : A.Array (A.Array Cell)
        initialGrid =
            A.initialize numRows (\row ->
                A.initialize numCols (\col ->
                    Cell row col ""
                )
            )
    in
    Model numRows numCols initialGrid ({x = 0, y = 0}, {x = 0, y = 0}) False

type Msg = 
    PressCell Coord
  | ReleaseMouse
  | HoverOver Coord Bool

stylesheet : Html.Html msg
stylesheet =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      "//maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
            ]
        children = []
    in 
        node tag attrs children

update : Msg -> Model -> Model
update msg model =
    case msg of
        PressCell { x, y } -> { model | selectedRange = ({ x = x, y = y }, { x = x, y = y }), clickPressed = True }
        ReleaseMouse -> {model | clickPressed = False }
        HoverOver { x, y } clickPressed ->
            case clickPressed of
                True  -> case model.selectedRange of
                    (start, _) -> { model | selectedRange = (start, { x = x, y = y }) }
                False -> model

rowToHtmlWithIndex : Int -> Model -> Html.Html Msg
rowToHtmlWithIndex rIndex model =
    let 
        sideTag = 
            Html.td sideBorderStyle [ Html.text (String.fromInt (rIndex + 1))]
        
        cells = List.indexedMap (\cIndex _ -> 
            let 
                cellValue = Maybe.andThen (\row -> A.get rIndex row) (A.get cIndex model.values)
                cellContent = Maybe.withDefault "" (Maybe.map (\x -> x.content) cellValue)
                cellStyleAttributes = Maybe.map (\cell -> cellStyle cell model.selectedRange) cellValue |> Maybe.withDefault []
            in
            Html.td (
                 --add events to cells
                   (onMouseDown (PressCell { x = cIndex, y = rIndex})) -- select begin
                ::  onMouseUp ReleaseMouse -- select end
                :: (onMouseOver (HoverOver { x = cIndex, y = rIndex} model.clickPressed)) -- drag/hover, depends on clickPressed
                :: cellStyleAttributes) [ Html.text cellContent ]) -- to the base style
            (List.range 0 (model.max_y - 1))
    in
    Html.tr [] (sideTag :: cells)


isBetween : Coord -> Coord -> Int -> Int -> Bool
isBetween startCoord endCoord cx cy =
    let
        minX = min startCoord.x endCoord.x
        minY = min startCoord.y endCoord.y
        maxX = max startCoord.x endCoord.x
        maxY = max startCoord.y endCoord.y
    in
    cx >= minX && cx <= maxX && cy >= minY && cy <= maxY

cellStyle : Cell -> (Coord, Coord) -> List (Html.Attribute msg)
cellStyle cell (startCoord, endCoord) =
    let
        baseStyle =
            [ Html.Attributes.style "border" "1px solid #ccc" -- Light gray border
            , Html.Attributes.style "padding" "8px"
            ]

        selectedStyle =
            if isBetween startCoord endCoord cell.pos_x cell.pos_y then
                [Html.Attributes.style "background" "gray"]
            else
                []
    in
    baseStyle ++ selectedStyle

sideBorderStyle : List (Html.Attribute msg)
sideBorderStyle =
    [ Html.Attributes.style "border" "1px solid #999" -- Dark gray border
    , Html.Attributes.style "padding" "8px"
    , Html.Attributes.style "background" "#f5f5f5" -- Light gray background
    , Html.Attributes.style "max-width" "40px"
    , Html.Attributes.style "min-height" "20px"
    , Html.Attributes.style "height" "20px"
    , Html.Attributes.style "min-width" "40px"
    , Html.Attributes.style "position" "sticky" -- Make it sticky
    , Html.Attributes.style "left" "0" -- Stick to the left
    , Html.Attributes.style "top" "100px" -- Stick 100px away from the top
    , Html.Attributes.style "user-select" "none"
    ]

headBorderStyle : List (Html.Attribute msg)
headBorderStyle =
    [ Html.Attributes.style "border" "1px solid #999" -- Dark gray border
    , Html.Attributes.style "padding" "8px"
    , Html.Attributes.style "background" "#f5f5f5" -- Light gray background
    , Html.Attributes.style "min-width" "40px"
    , Html.Attributes.style "height" "20px"
    , Html.Attributes.style "position" "sticky" -- Make it sticky
    , Html.Attributes.style "top" "100px" -- Stick 100px away from the top
    , Html.Attributes.style "left" "0" -- Stick to the left
    , Html.Attributes.style "z-index" "1" -- Ensure the top border is on top
    , Html.Attributes.style "user-select" "none"
    ]


alphabeticalTags : Int -> List String
alphabeticalTags n =
    List.map (\i -> convertToBase26 (i + 1)) (List.range 0 (n - 1))

convertToBase26 : Int -> String
convertToBase26 n =
    if n <= 0 then
        ""
    else
        let
            remainder = Basics.modBy 26 (n - 1) 
            quotient = (n - 1) // 26
        in
        convertToBase26 quotient ++ String.fromChar (Char.fromCode (65 + remainder))

createHeader : Int -> List (Html.Html msg)
createHeader numColumns =
    let
        headers =
            alphabeticalTags numColumns
        corner = Html.th [
          Html.Attributes.style "border" "1px solid black",
          Html.Attributes.style "background" "#d4d4d4"
          ] []
    in
    corner :: (List.map (\header -> Html.th headBorderStyle [Html.text header]) headers)

myTable : Model -> Html.Html Msg
myTable model =
    Html.table [
       Html.Attributes.style "border-collapse" "collapse", 
       Html.Attributes.style "width" "100%",
       Html.Attributes.style "margin-top" navbarHeight
    ]
        [ Html.thead [ 
          Html.Attributes.style "border-collapse" "collapse", 
          Html.Attributes.style "width" "100%"
        ]
        (createHeader model.max_x)
        , Html.tbody [
          Html.Attributes.style "text-align" "center"
        ] 
        (List.indexedMap (\index _ -> rowToHtmlWithIndex index model) (List.range 0 (model.max_y - 1)))
        ]

 
navbarHeight : String
navbarHeight = "100px"

navbar : Html.Html msg
navbar =
    Html.div
        [ Html.Attributes.style "position" "fixed"
        , Html.Attributes.style "height" navbarHeight
        , Html.Attributes.style "background" "#d1ffdd"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "top" "0px"
        , Html.Attributes.style "background-image" "linear-gradient(to bottom, #d1ffdd, #7ce5b8)"
        ]
        [ Html.text "Excel, supposedly" ]

view : Model -> Html.Html Msg
view model = Html.div [Html.Attributes.style "user-drag" "none"] [
  navbar,
  myTable model
  ]

toRow : A.Array (Maybe Int) -> Html.Html Msg 
toRow r = Html.tr []
  (A.toList (A.map toCell r))

toCell : Maybe Int -> Html.Html Msg 
toCell m = case m of 
  Nothing -> Html.th [] []
  Just i  -> Html.th [] [
    Html.text (String.fromInt  i)
    ]
