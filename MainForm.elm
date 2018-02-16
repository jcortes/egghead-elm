module MainForm exposing (..)

import Html exposing (program, Html, div, h1, text, input, button, textarea)
import Html.Attributes exposing (placeholder, type_, rows, value, class, disabled, required)
import Html.Events exposing (onInput, onClick, onSubmit)
import Http exposing (jsonBody)
import Json.Encode as Encode
import Json.Decode as Decode

type alias Model =
  { email : String
  , message : String
  , status : SubmissionStatus }

type SubmissionStatus
  = NotSubmitted
  | InProcess
  | Succeeded
  | Failed

type Msg
  = InputEmail String
  | InputMessage String
  | Submit
  | SubmitResponse (Result Http.Error ())
  | Cancel

initialModel : Model
initialModel =
  { email = ""
  , message = ""
  , status = NotSubmitted }

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
      ({ model | email = String.toLower newEmail }, Cmd.none)
    InputMessage newMsg ->
      ({ model | message = newMsg }, Cmd.none)
    Submit ->
      ({ model | status = InProcess }, submit model)
    SubmitResponse (Ok ()) ->
      ({ model | status = Succeeded
        , email = ""
        , message = ""
      }, Cmd.none)
    SubmitResponse (Err _) ->
      ({ model | status = Failed }, Cmd.none)
    Cancel ->
      ({ model | status = NotSubmitted
        , email = ""
        , message = ""
      }, Cmd.none)

submit : Model -> Cmd Msg
submit model =
  let
    url = "http://localhost:3000/api/contact"
    
    json = Encode.object
      [ ("email", Encode.string model.email)
      , ("message", Encode.string model.message) ]
    
    decoder = Decode.string |> Decode.map (always ())

    request : Http.Request ()
    request = Http.post url (jsonBody json) decoder
  in
    request |> Http.send SubmitResponse

renderStatus : SubmissionStatus -> Html a
renderStatus status =
  case status of
    NotSubmitted ->
      div [] []
    InProcess ->
      div [] [text "Your request is being sent"]
    Succeeded ->
      div [] [text "Your request has been received"]
    Failed ->
      div [class "alert alert-danger"]
        [text "Ops! There was an error, please try again"]

view : Model -> Html Msg
view model =
  Html.form
    [ onSubmit Submit ]
    [ header model
    , body model
    , footer model
    , div [] [ model |> toString |> text ]
    ]

header : Model -> Html a
header model =
  div []
    [ h1 [] [ text "Contact us" ]
    , renderStatus model.status
    ]

body : Model -> Html Msg
body model =
  div [] [
    div [] [
      input [
        placeholder "your email",
        type_ "email",
        onInput InputEmail,
        value model.email,
        required True
      ] []
    ],
    div [] [
      textarea [
        placeholder "your message",
        rows 7,
        onInput InputMessage,
        value model.message,
        required True
      ] []
    ]
  ]

footer : Model -> Html Msg
footer model =
  div [] [
    button [
      type_ "submit",
      disabled (model.status == InProcess)
    ]
    [ text "Submit!" ],
    button [
      type_ "button",
      onClick Cancel
    ]
    [ text "Cancel" ]
  ]