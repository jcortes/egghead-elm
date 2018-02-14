module MainForm exposing (..)

import Html exposing (program, Html, div, h1, text, input, button, textarea)
import Html.Attributes exposing (placeholder, type_, rows)
import Html.Events exposing (onInput)

type alias Model =
  { email : String
  , message : String }

type Msg
  = InputEmail String
  | InputMessage String
  | Submit

initialModel : Model
initialModel =
  { email = ""
  , message = "" }

main : Program Never Model Msg
main =
  program
    { init = (initialModel, Cmd.none)
    , update = update
    , subscriptions = \model -> Sub.none
    , view = view
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    InputEmail newEmail ->
      ({ model | email = newEmail }, Cmd.none)
    InputMessage newMsg ->
      ({ model | message = newMsg }, Cmd.none)
    Submit ->
      (model, Cmd.none)

view : Model -> Html Msg
view model =
  Html.form [] [
    header
    , body
    , footer
    , div [] [ model |> toString |> text ]
  ]

header : Html a
header =
  div [] [
    h1 [] [ text "Contact us" ]
  ]

body : Html Msg
body =
  div [] [
    div [] [
      input [
        placeholder "your email",
        type_ "email",
        onInput InputEmail
      ] []
    ],
    div [] [
      textarea [
        placeholder "your message",
        rows 7,
        onInput InputMessage
      ] []
    ]
  ]

footer : Html a
footer =
  div [] [
    button [] [ text "Submit!" ]
  ]