module UnionTypes exposing (..)

import Html exposing (text, div, br)

rating : Rating
rating = Excellent

breed : Breed
breed = Crossbreed Husky (Crossbreed Poodle Beagle)

main =
  div []
    [ text (renderRating rating)
    , br [] []
    , text (toString breed)
    , br [] []
    , text (renderBoolean True) ]

type Breed
  = Beagle
  | Husky
  | Poodle
  | Crossbreed Breed Breed

type Rating
  = Excellent
  | Good
  | Poor
  | Other String

renderRating : Rating -> String
renderRating rating =
  case rating of
    Other s -> s
    _ -> toString  rating

renderBoolean : Bool -> String
renderBoolean b =
  case b of
    True -> "Yep"
    False -> "Nah"