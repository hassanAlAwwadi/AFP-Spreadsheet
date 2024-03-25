module Index exposing (..)
import Array as A
import Browser
import Html.Attributes exposing (attribute)
import Html exposing (node)
import Model exposing (Model, Cell)

main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }

init : Model
init = 
    let
        defaultCellValue = Cell "" False
        initialRow = A.repeat 100 defaultCellValue
        initialValue = A.repeat 100 initialRow
    in
    Model 100 100 initialValue

type alias Msg = 
  { x : Int
  , y : Int 
  , v : Cell
  }

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
  let mx  = A.get msg.x model.values
  in 
  case mx of 
    Nothing -> model
    Just xr -> 
      let 
        xrp = A.set msg.y msg.v xr
        vlp = A.set msg.x xrp model.values
      in { model | values = vlp}


tableData : List (List String)
tableData =
    [ ["Name", "Age", "Country"]
    , ["John", "30", "USA"]
    , ["Alice", "25", "Canada"]
    , ["Bob", "35", "UK"]
    ]

rowToHtmlWithIndex : Int -> Model -> Html.Html msg
rowToHtmlWithIndex rIndex model =
    let 
        sideTag = 
            Html.td sideBorderStyle [ Html.text (String.fromInt (rIndex + 1))]
        
        cells = List.indexedMap (\cIndex _ -> 
            let 
                cellValue = Maybe.andThen (\row -> A.get rIndex row) (A.get cIndex model.values)
                cellContent = Maybe.withDefault "" (Maybe.map (\x -> x.content) cellValue)
            in
            Html.td cellStyle [ Html.text cellContent ]) (List.range 0 (model.max_y - 1))
    in
    Html.tr [] (sideTag :: cells)


cellStyle : List (Html.Attribute msg)
cellStyle =
    [ Html.Attributes.style "border" "1px solid #ccc" -- Light gray border
    , Html.Attributes.style "padding" "8px"
    ]

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
    , Html.Attributes.style "z-index" "1" -- Ensure the top border is on top
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

myTable : Model -> Html.Html msg
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
view model = Html.div [] [
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
