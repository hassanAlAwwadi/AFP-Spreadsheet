module Index exposing (..)
import Array as A
import Browser
import Html.Attributes
import Model exposing (..)
import Navbar exposing (navbar)
import Borders exposing (..)
import Cell exposing (..)
import Table exposing (..)
import Html

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

update : Msg -> Model -> Model
update msg model =
    case msg of
        PressCell { x, y } -> { model | selectedRange = ({ x = x, y = y }, { x = x, y = y }), clickPressed = True }
        ReleaseMouse -> {model | clickPressed = False }
        AddColumns nr -> { model | max_x = model.max_x + nr}
        AddRows nr -> { model | max_y = model.max_y + nr}
        HoverOver { x, y } clickPressed ->
            if clickPressed 
            then
                case model.selectedRange of
                    (start, _) -> { model | selectedRange = (start, { x = x, y = y }) }
            else
                model

view : Model -> Html.Html Msg
view model = Html.div [Html.Attributes.style "user-drag" "none"] [
  navbar model,
  myTable model
  ]
