import Html exposing (..)
import Json.Decode exposing (..)
import Dict exposing (Dict)

main : Html msg
main =
  let
    navigationItems = ["about_us", "location", "terms_of_service"]

    dict = json
      |> decodeString (Json.Decode.dict string)
      |> Result.withDefault Dict.empty

    translate key = Dict.get key dict |> Maybe.withDefault key
  in
    ul []
      (navigationItems
        |> List.map translate
        |> List.map (\item -> li [] [ text item ]))

json : String
json = """
{
  "about_us": "Acerca de nosotros",
  "location": "Ubicación",
  "terms_of_service": "Políticas",
  "house_rules": "Reglas de la casa",
  "map": "Mapa",
  "directions": "Cómo llegar",
  "contact": "Contacto"
}
"""
