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
import Cmd exposing (sendData)
import Json.Decode as Decode
import Browser.Events

main : Program () Model Msg
main =
  Browser.element { init = \_ -> (init, Cmd.none), update = update, view = view, subscriptions = subscriptions }

subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown keyDecoder

keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)

toKey : String -> Msg
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            PressedLetter char
        _ ->
            PressedControl string

init : Model
init = 
    let
        numRows = 100
        numCols = 100
        initialGrid : A.Array (A.Array Cell)
        initialGrid =
            A.initialize numRows (\row ->
                A.initialize numCols (\col ->
                    Cell row col "" (Ok "")
                )
            )
    in
    Model numRows numCols initialGrid ({x = 0, y = 0}, {x = 0, y = 0}) False Nothing Nothing

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PressCell { x, y } -> ({ model | selectedRange = ({ x = x, y = y }, { x = x, y = y }), clickPressed = True, editingCell = Nothing }, Cmd.none)
        ReleaseMouse -> ({model | clickPressed = False }, Cmd.none)
        AddColumns nr -> ({ model | max_x = model.max_x + nr}, Cmd.none)
        AddRows nr -> ({ model | max_y = model.max_y + nr}, Cmd.none)
        PressTopBorder x -> ({ model | selectedRange = ({x = x, y = 0}, {x = x, y = model.max_y}), clickPressed = True, editingCell = Nothing}, Cmd.none)
        PressBotBorder y -> ({ model | selectedRange = ({x = 0, y = y}, {x = model.max_x, y = y}), clickPressed = True, editingCell = Nothing}, Cmd.none)
        EditModeCell cell -> ({ model | editingCell = Just cell, clipboard = Nothing}, Cmd.none)
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
        EditCellUpdate x y content -> 
            let
                updatedGrid =
                    case A.get x model.values of
                           Nothing -> model.values -- x index out of bounds, return original grid
                           Just row ->
                                case A.get y row of
                                Nothing -> model.values -- y index out of bounds, return original grid
                                Just cell ->
                                    let
                                        updatedCell = { cell | content = Maybe.withDefault "" (Just content)}
                                        updatedRow = A.set y updatedCell row
                                    in
                                    A.set x updatedRow model.values
            in
            ({ model | values = updatedGrid}, Cmd.none)
        ConfirmEdit -> confirmEdit model
        ResponseServer r -> case r of 
            Err _       -> (model, Cmd.none)
            Ok either -> case either of 
                Err ((x,y), err) -> case A.get x model.values of
                           Nothing -> (model,  Cmd.none)
                           Just row ->
                                case A.get y row of
                                Nothing -> (model,  Cmd.none)
                                Just cell -> 
                                    let
                                        new_row = A.set y { cell | value =  Err err} row
                                        new_table = { model | values = A.set x new_row model.values }
                                    in  (new_table, Cmd.none)
                                
                Ok values ->
                    let 
                        newvalues = insertAllValues values model.values
                    in ({model | values = newvalues}, Cmd.none)
        Whatever r -> (model, Cmd.none)
        PressedLetter char ->
            if char == 'c' && model.editingCell == Nothing then ({model | clipboard = Just model.selectedRange}, Cmd.none) else (model, Cmd.none)
        PressedControl string -> 
            if string == "Enter" then confirmEdit model else (model, Cmd.none)


insertAllValues: List (( Int, Int ), String) -> A.Array (A.Array Cell) -> A.Array (A.Array Cell)
insertAllValues new values = case new of 
    [] -> values 
    (((x,y),val) :: rest) -> case A.get x values of
                           Nothing -> insertAllValues rest values
                           Just row ->
                                case A.get y row of
                                Nothing -> insertAllValues rest values
                                Just cell -> 
                                    let
                                        new_row = A.set y { cell | value =  Ok val} row
                                        new_values = A.set x new_row values
                                    in  insertAllValues rest new_values
confirmEdit : Model -> (Model, Cmd Msg)
confirmEdit model = 
    let
       newModel = { model | editingCell = Nothing  }
    in
       ( newModel, sendData model)
view : Model -> Html.Html Msg
view model = Html.div [Html.Attributes.style "user-drag" "none"] [
  navbar model,
  myTable model
  ]
