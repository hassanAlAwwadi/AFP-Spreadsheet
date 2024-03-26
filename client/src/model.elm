module Model exposing (Model, Cell, Coord)

import Array as A

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

