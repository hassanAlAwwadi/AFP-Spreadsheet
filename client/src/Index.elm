module Index exposing (..)
import Html
import Array as A
import Browser
import Html.Attributes

main =
  Browser.sandbox { init = init, update = update, view = view }


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


view : Model -> Html.Html Msg
view model = Html.table [{--Html.Attribute ["1px" ,"solid" ,"black"]... how do I do you :sob:--}]
  (A.toList (A.map toRow model.values))

toRow : A.Array (Maybe Int) -> Html.Html Msg 
toRow r = Html.tr []
  (A.toList (A.map toCell r))

toCell : Maybe Int -> Html.Html Msg 
toCell m = case m of 
  Nothing -> Html.th [] []
  Just i  -> Html.th [] [Html.text (String.fromInt  i)]
