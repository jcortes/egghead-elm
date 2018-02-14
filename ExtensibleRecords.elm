module ExtensibleRecords exposing (..)

import Html exposing (text)

main =
  text (joe |> rename "John" |> greet |> toString )

type alias Named r = { r | name : String }

greet : Named r  -> String
greet p = "Hello " ++ p.name

rename : String -> Named r -> Named r
rename newName r = { r | name = newName }

type alias Dog =
  { name : String
  , breed : String }

type alias Person =
  { name : String
  , occupation : String }

fido : Dog
fido = Dog "Fido" "Husky"

joe : Person
joe = Person "Joe" "Developer"
