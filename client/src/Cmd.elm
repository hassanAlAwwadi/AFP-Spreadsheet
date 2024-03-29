module Cmd exposing (..)

import Model exposing (..)
import Http
import Json.Encode as E
import Array exposing (Array)

sendDataCmd : Model -> Cmd Msg
sendDataCmd m = 
    let
        jsonBody = E.object
            [ ("max_x", E.int m.max_x),
              ("max_y", E.int m.max_y),
              ("cells", encodeSS m.values ) ] -- for now, just this
    in
    Http.post 
        { body = Http.jsonBody jsonBody
        , expect = Http.expectJson ResponseServer responseDecoder
        , url = "http://localhost:31415/raw" }

encodeCell : Cell -> E.Value
encodeCell c = E.object
                [ ("pos_x", E.int c.pos_x),
                  ("pos_y", E.int c.pos_y),
                  ("content", E.string c.content) ]

encodeSS : Array (Array Cell) -> E.Value
encodeSS aac = E.array (E.array encodeCell) aac

responseDecoder = Debug.todo "Yes, I will need to receive the processed response and deconstruct it"