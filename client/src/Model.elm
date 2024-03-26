module Model exposing (..)

import Array as A

type Msg = 
    PressCell Coord
  | ReleaseMouse
  | HoverOver Coord Bool
  | AddRows Int
  | AddColumns Int

type alias Coord = { 
  x : Int,
  y : Int
  }

type alias Cell =
    { 
      pos_x : Int
    , pos_y : Int
    , content : String
    }

type alias Model =
  { max_x  : Int 
  , max_y  : Int
  , values : A.Array (A.Array (Cell))
  , selectedRange : (Coord, Coord)
  , clickPressed : Bool
  }

