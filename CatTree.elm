import Html exposing (Html, text, li, ul)
import Json.Decode exposing (..)

main : Html msg
main =
  case json |> decodeString node of
    Ok (Node _ children) -> render children
    Err err -> text err

type Node = Node String (List Node)

node : Json.Decode.Decoder Node
node =
  Json.Decode.map2
    Node
      (field "value" string)
      ((field "children" (list (lazy (\_ -> node))))
        |> maybe
        |> Json.Decode.map (Maybe.withDefault []))

render : List Node -> Html msg
render nodes =
  let
    renderNode (Node value children) =
      li [] [ text value, render children ]
  in
    case nodes of
      [] -> text ""
      _ -> ul [] (nodes |> List.map renderNode)

json : String
json = """
{
  "value" : "root",
  "children" : [{
    "value" : "Bikes",
    "children" : [
      { "value" : "Cruiser bikes" },
      { "value" : "Road bikes" },
      { "value" : "MTB bikes" }
    ]
  }, {
    "value" : "Components",
    "children" : [
      { "value": "Saddles" },
      {
        "value": "Brakes",
        "children": [
          { "value": "Brake levers" },
          { "value": "Brake cables" },
          { "value": "Brake pads" }
        ]
      },
      { "value": "Cassettes" },
      { "value": "Chains" },
      { "value": "Pedals" },
      { "value": "Stems" }
    ]
  }, {
    "value" : "Wheels & tyres",
    "children" : [
      { "value" : "Hubs" },
      { "value" : "Spokes" },
      { "value" : "Rims" },
      {
        "value" : "Tyres",
        "children" : [
          { "value" : "Cruiser tyres" },
          { "value" : "Road tyres" },
          { "value" : "MTB tyres" }
        ]
      }
    ]
  }]
}
"""