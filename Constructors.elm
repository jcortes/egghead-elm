module Constructors exposing (..)

import Html exposing (text, div, br, h1)
import Email exposing (..)

type alias Message =
  { recipient : Email.Address
  , body : String }

data : String
data = "hello@"

main =
  let content =
    data
    |> Email.validate
    |> Result.map Email.toString
    |> Result.withDefault "Invalid"
  in
    div []
      [ h1 [] [text content]
      , br [] [] ]
