module Cmd exposing (..)

import Model exposing (..)
import Http
import Json.Encode as E
import Array as A 
import Json.Decode exposing (Decoder, field, index, int, string, oneOf, map, map2)
import Json.Decode exposing (list)

sendData : Model -> Cmd Msg
sendData m = case m.editingCell of 
  Nothing -> Http.cancel "No message required"
  Just {x, y} -> case A.get x m.values of
    Nothing -> Http.cancel "updated row out of table range"
    Just row -> case A.get y row of 
      Nothing -> Http.cancel "updated column out of table range"
      Just cell -> 
        let 
          jsonBody = E.object
            [ ("max_x", E.int m.max_x),
              ("max_y", E.int m.max_y),
              ("cell", encodeCell cell) ] -- only encode the updated cell
        in Http.post 
          { body = Http.jsonBody jsonBody
          , expect =  Http.expectJson ResponseServer (responseDecoder x y)
          , url = "http://localhost:31415/raw" }

encodeCell : Cell -> E.Value
encodeCell c = E.object
                [ ("pos_x", E.int c.pos_x),
                  ("pos_y", E.int c.pos_y),
                  ("content", E.string c.content) ]

encodeSS : A.Array (A.Array Cell) -> E.Value
encodeSS aac = E.array (E.array encodeCell) aac

responseDecoder : Int -> Int -> Decoder (Result ((Int,Int), String) (List ((Int,Int), String)))
responseDecoder x y = oneOf 
  [
    map Ok (map (List.map (\((v, w), val) -> ((v,w),val))) 
      (list 
        (map2 (Tuple.pair) 
          (index 0 (map2 (Tuple.pair) 
            (index 0 int) 
            (index 1 int))) 
          (map String.fromInt (index 1 int)))))
  , map (\s -> Err ((x,y), s)) string

  ]