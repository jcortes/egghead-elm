import Html exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Extra exposing
  ((|:), date, fromResult, parseInt)
import Json.Decode.Pipeline exposing
  (decode, optional, optionalAt, required, requiredAt)
import Date exposing (Date)

-- main : Html msg
-- main = json
--   |> decodeString userDecoder
--   |> toString 
--   |> text

-- main : Html msg
-- main = sampleFailure
--   |> decodeString response
--   |> toString 
--   |> text

main : Html msg
main = sampleSuccess
  |> decodeString response
  |> toString 
  |> text

type Membership = Standard | Premium

type Gender = Male | Female

type alias Notification =
  { title : String
  , message : String
  }

type alias User =
  { id : Int
  , email : String
  , membership : Membership
  , gender : Maybe Gender
  , dateOfBirth : Date
  , notifications : List Notification
  }

json : String
json = """
{
  "id": "123",
  "email": "Jacortesmahmud@gmail.com",
  "membership": true,
  "profile": {
    "gender": "male",
    "dateOfBirth": "Sun Jan 07 2018 00:18:17 GMT+0100 (CET)"
  },
  "notifications": [
    { "title": "Welcome back", "message": "We've been missing you" },
    { "title": "Weather alert", "message": "Expect stormy weather" }
  ]
}
"""

sampleSuccess : String
sampleSuccess = """
{
  "data": {
    "id": "123",
    "email": "Jacortesmahmud@gmail.com",
    "membership": true,
    "profile": {
      "gender": "male",
      "dateOfBirth": "Sun Jan 07 2018 00:18:17 GMT+0100 (CET)"
    },
    "notifications": [
      { "title": "Welcome back", "message": "We've been missing you" },
      { "title": "Weather alert", "message": "Expect stormy weather" }
    ]
  }
}
"""

sampleFailure : String
sampleFailure = """
{
  "error": {
    "message": "wrong_password"
  }
}
"""

response : Decoder (Result String User)
response =
  oneOf
    [ field "data" userDecoder |> Json.Decode.map Ok
    , at [ "error", "message" ] string |> Json.Decode.map Err
    ]

membership : Decoder Membership
membership =
  let
    toMembership b =
      case b of
        True -> Premium
        False -> Standard
  in
    bool |> Json.Decode.map toMembership

gender : Decoder Gender
gender =
  let
    toGender s =
      case s of
        "male" -> succeed Male
        "female" -> succeed Female
        _ -> fail (s ++ " is not a valid gender")
  in
    string |> Json.Decode.andThen toGender

notification : Decoder Notification
notification =
  decode Notification
    |> required "title" string
    |> optional "message" string ""

userDecoder : Decoder User
userDecoder =
  decode User
    |> required "id" parseInt
    -- |> required "id" (string |> Json.Decode.andThen (String.toInt >> fromResult))
    |> required "email" (string |> Json.Decode.map String.toLower)
    |> required "membership" membership
    |> optionalAt ["profile", "gender"] (gender |> Json.Decode.map Just) Nothing
    |> requiredAt ["profile", "dateOfBirth"] date
    -- |> optional "gender" gender ""
    -- |> (field "dateOfBirth" date)
    |> optional "notifications" (list notification) []
