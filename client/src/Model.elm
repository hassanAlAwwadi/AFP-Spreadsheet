module Model exposing (..)

import Array as A
import Http

type Msg = 
    PressCell Coord
  | ReleaseMouse
  | PressTopBorder Int
  | PressBotBorder Int
  | HoverOverTopBorder Int Bool
  | HoverOverBotBorder Int Bool
  | HoverOver Coord Bool
  | EditModeCell Coord
  | EditCellUpdate Int Int String
  | AddRows Int
  | AddColumns Int
  | ConfirmEdit
  | ResponseServer (Result Http.Error (Result ((Int,Int), String) (List ((Int,Int), String))))
  | Whatever (Result Http.Error ())
  | PressedLetter Char
  | PressedControl String

type alias Coord = { 
  x : Int,
  y : Int
  }

type alias CellResponse = { xy : Coord, value : Int }

type alias Cell =
    { 
      pos_x : Int
    , pos_y : Int
    , content : String
    , value   : Result String String
    }

type alias Model =
  { max_x  : Int 
  , max_y  : Int
  , values : A.Array (A.Array (Cell))
  , selectedRange : (Coord, Coord)
  , clickPressed : Bool
  , editingCell : Maybe Coord
  , clipboard : Maybe (Coord, Coord)
  }

