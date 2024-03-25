module Model exposing (Model, Cell)

import Array as A

type alias Cell =
    { content : String
    , selected : Bool
    }

type alias Model =
  { max_x  : Int 
  , max_y  : Int
  , values : A.Array (A.Array (Cell))
  }

