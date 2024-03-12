module Index exposing (..)
import Html
import Array as A

main =
  Html.text "Hello!"


type alias Model =
  { max_x  : Int 
  , max_y  : Int
  , values : A.Array (A.Array (Maybe Int))
  }


init : Model
init =
  Model 100 100 (A.initialize 100 (\_ -> A.initialize 100  (\_ -> Nothing)))

type alias Msg = 
  { x : Int
  , y : Int 
  , v : Maybe Int
  }


update : Msg -> Model -> Model
update ({ x, y, v }) ({max_x, max_y, values} as m) = 
  let mx  = A.get x values
  in 
  case mx of 
    Nothing -> m
    Just xr -> 
      let 
        xrp = A.set y v xr
        vlp = A.set x xrp values
      in Model max_x max_y vlp

