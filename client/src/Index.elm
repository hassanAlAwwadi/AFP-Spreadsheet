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
import Cmd exposing (sendDataCmd)

main : Program () Model Msg
main =
  Browser.element { init = \_ -> (init, Cmd.none), update = update, view = view, subscriptions = \_ -> Sub.none }

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
    Model numRows numCols initialGrid ({x = 0, y = 0}, {x = 0, y = 0}) False Nothing

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PressCell { x, y } -> ({ model | selectedRange = ({ x = x, y = y }, { x = x, y = y }), clickPressed = True, editingCell = Nothing }, Cmd.none)
        ReleaseMouse -> ({model | clickPressed = False }, Cmd.none)
        AddColumns nr -> ({ model | max_x = model.max_x + nr}, Cmd.none)
        AddRows nr -> ({ model | max_y = model.max_y + nr}, Cmd.none)
        PressTopBorder x -> ({ model | selectedRange = ({x = x, y = 0}, {x = x, y = model.max_y}), clickPressed = True, editingCell = Nothing}, Cmd.none)
        PressBotBorder y -> ({ model | selectedRange = ({x = 0, y = y}, {x = model.max_x, y = y}), clickPressed = True, editingCell = Nothing}, Cmd.none)
        EditModeCell cell -> ({ model | editingCell = Just cell }, Cmd.none)
        HoverOverTopBorder x clickPressed -> if clickPressed 
            then
                case model.selectedRange of
                    (start, end) -> ({ model | selectedRange = (start, { x = x, y = end.y }) }, Cmd.none)
            else
                (model, Cmd.none)
        HoverOverBotBorder y clickPressed -> if clickPressed 
            then
                case model.selectedRange of
                    (start, end) -> ({ model | selectedRange = (start, { x = end.x, y = y }) }, Cmd.none)
            else
                (model, Cmd.none)
        HoverOver { x, y } clickPressed ->
            if clickPressed 
            then
                case model.selectedRange of
                    (start, _) -> ({ model | selectedRange = (start, { x = x, y = y }) }, Cmd.none)
            else
                (model, Cmd.none)
        SendDataEnter -> (model, sendDataCmd model)
        ResponseServer r -> (Debug.todo "Response not handled yet", Cmd.none)

view : Model -> Html.Html Msg
view model = Html.div [Html.Attributes.style "user-drag" "none"] [
  navbar model,
  myTable model
  ]
