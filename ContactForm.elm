module ContactForm exposing (..)

import Html exposing
  (program, Html, div, h1,
  text, input, button, textarea,
  label)
import Html.Attributes exposing
  (placeholder, type_, rows, value,
  class, disabled, required, novalidate)
import Html.Events exposing
  (onInput, onClick, onSubmit, onCheck)
import Http exposing (jsonBody)
import Json.Encode as Encode
import Json.Decode as Decode

import Validation exposing (..)

type alias Model =
  { email : Field String String
  , message : Field String String
  , age : OptionalField String Int
  , acceptPolicy : Field Bool Bool
  , status : SubmissionStatus }

type SubmissionStatus
  = NotSubmitted
  | InProcess
  | Succeeded
  | Failed

type Msg
  = InputEmail String
  | InputMessage String
  | InputAge String
  | CheckAcceptPolicy Bool
  | Submit
  | SubmitResponse (Result Http.Error ())
  | Cancel

initialModel : Model
initialModel =
  { email = field ""
  , message = field ""
  , age = field ""
  , acceptPolicy = field False
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
    InputEmail incomingEmail ->
      ({ model | email = field incomingEmail }, Cmd.none)
    InputMessage incomingMsg ->
      ({ model | message = field incomingMsg }, Cmd.none)
    InputAge incomingAge ->
      ({ model | age = field incomingAge }, Cmd.none)
    CheckAcceptPolicy incomingCheck ->
      ({ model | acceptPolicy = field incomingCheck }, Cmd.none)
    Submit ->
      model |> validateModel |> submitIfValid
    SubmitResponse (Ok ()) ->
      ({ initialModel | status = Succeeded }, Cmd.none)
    SubmitResponse (Err _) ->
      ({ model | status = Failed }, Cmd.none)
    Cancel ->
      ({ initialModel | status = NotSubmitted }, Cmd.none)

submitIfValid : Model -> (Model, Cmd Msg)
submitIfValid model =
  let
    submissionResult : Validity (Cmd Msg)
    -- submissionResult =
    --   Valid submit
    --     |> apply model.email
    --     |> apply model.message
    --     |> apply model.age
    submissionResult =
      Valid submit
        |: (validity model.email)
        |: (validity model.message)
        |: (validity model.age)
        |: (validity model.acceptPolicy)
  in
    case submissionResult of
      Valid cmd ->
        ({ model | status = InProcess }, cmd)
      _ ->
        (model, Cmd.none)

validateModel : Model -> Model
validateModel model =
  let
    emailValidation =
      isNotEmpty "An email is required"
      >=> isEmail "Please ensure this is a valid email"
    email = model.email |> validate emailValidation

    message = model.message
      |> validate (isNotEmpty "A message is required")
    
    age = model.age
      |> validate (optional (isNatural "I'm expecting a positive number"))
    
    acceptPolicy = model.acceptPolicy
      |> validate (isTrue "You must accept the policy")
  in
    { model | email = email
            , message = message
            , age = age
            , acceptPolicy = acceptPolicy }

submit : String -> String -> Maybe Int -> Bool -> Cmd Msg
submit email message age _ =
  let
    url = "http://localhost:3000/api/contact"
    
    json = Encode.object
      [ ("email", Encode.string email)
      , ("message", Encode.string message)
      , ("age", age |> Maybe.map Encode.int
                    |> Maybe.withDefault Encode.null) ]
    
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

errorLabel : Field raw a -> Html Msg
errorLabel field = label
  [ class "label label-error" ]
  [ field
    |> extractError
    |> Maybe.withDefault ""
    |> text
  ]

view : Model -> Html Msg
view model =
  Html.form
    [ onSubmit Submit
    , novalidate True ]
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
  div []
    [ div []
      [ input
        [ placeholder "your email *"
        , type_ "email"
        , onInput InputEmail
        -- , value (model.email |> displayValue identity)
        , value (model.email |> rawValue)
        , required True
        ] []
      , errorLabel model.email
      ]
    , div []
      [ textarea
        [ placeholder "your message *"
        , rows 7
        , onInput InputMessage
        -- , value (model.message |> displayValue identity)
        , value (model.message |> rawValue)
        , required True
        ] []
      , errorLabel model.message
      ]
    , div []
      [ input
        [ placeholder "your age"
        , onInput InputAge
        -- , value (model.age |> displayValue (Maybe.map toString >> Maybe.withDefault ""))
        , value (model.age |> rawValue)
        ] []
      , errorLabel model.age
      ]
    , div []
      [ input
        [ placeholder "your age"
        , type_ "checkbox"
        , onCheck CheckAcceptPolicy
        -- , value (model.age |> displayValue (Maybe.map toString >> Maybe.withDefault ""))
        , value (model.acceptPolicy |> rawValue |> toString)
        ] []
      , label [] [ text "I accept the privacy policy" ]
      ]
    , div [] [ errorLabel model.acceptPolicy ]
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
