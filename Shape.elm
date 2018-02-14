module Shape exposing (..)

import Html exposing (text, div, br)

main =
  div []
    [ text (cone |> volume |> toString)
    , br [] []
    ,  text (box |> volume |> toString)
    , br [] []
    ,  text (sphere |> volume |> toString)]

cone = Cone { height = 2, radius = 1 }
box = Box { length = 1, height = 2, depth = 3 }
sphere = Sphere { radius = 3 }

type Shape
  = Sphere { radius: Float }
  | Box { length: Float, height: Float, depth: Float }
  | Cone { radius: Float,  height: Float }

volume : Shape -> Float
volume s =
  case s of
    Sphere { radius } ->
      4/3 * pi * radius ^ 3
    Box { length, height, depth } ->
      length * height * depth
    Cone { radius, height } ->
      pi * radius ^ 2 * height / 3
