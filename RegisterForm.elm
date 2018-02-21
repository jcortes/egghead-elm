module RegisterForm exposing (..)

import Html
    exposing
        ( program
        , Html
        , div
        , h1
        , text
        , input
        , button
        , textarea
        , label
        , ul
        , li
        )
import Html.Attributes
    exposing
        ( placeholder
        , type_
        , rows
        , value
        , class
        , disabled
        , required
        , novalidate
        )
import Html.Events exposing (onInput, onClick, onSubmit, onCheck, onBlur)
import Http
import Json.Encode as Encode
import Json.Decode as Decode
import Maybe.Extra exposing (toList)
import Validation exposing (..)


type alias Model =
    { email : Field String String
    , password : Field String String
    , confirmPassword : Field String String
    , acceptPolicy : Field Bool Bool
    , status : SubmissionStatus
    }


type SubmissionStatus
    = NotSubmitted
    | InProcess
    | Succeeded
    | Failed


type Msg
    = InputEmail String
    | BlurEmail
    | InputPassword String
    | BlurPassword
    | InputConfirmPassword String
    | BlurConfirmPassword
    | CheckAcceptPolicy Bool
    | Submit
    | SubmitResponse (Result Http.Error ResponseBody)
    | Cancel


initialModel : Model
initialModel =
    { email = field ""
    , password = field ""
    , confirmPassword = field ""
    , acceptPolicy = field False
    , status = NotSubmitted
    }


main : Program Never Model Msg
main =
    program
        { init = ( initialModel, Cmd.none )
        , update = update
        , subscriptions = \model -> Sub.none
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputEmail incomingEmail ->
            ( { model
                | email =
                    model.email
                        |> validate (OnChange incomingEmail) emailValidation
              }
            , Cmd.none
            )

        BlurEmail ->
            ( { model
                | email =
                    model.email
                        |> validate OnBlur emailValidation
              }
            , Cmd.none
            )

        InputPassword incomingPass ->
            let
                password =
                    model.password
                        |> validate (OnChange incomingPass) passwordValidation
            in
                ( { model
                    | password = password
                    , confirmPassword =
                        model.confirmPassword
                            |> validate OnRelatedChange (confirmPasswordValidation password)
                  }
                , Cmd.none
                )

        BlurPassword ->
            ( { model
                | password =
                    model.password
                        |> validate OnBlur passwordValidation
              }
            , Cmd.none
            )

        InputConfirmPassword incomingConfirmPass ->
            ( { model
                | confirmPassword =
                    model.confirmPassword
                        |> validate (OnChange incomingConfirmPass)
                            (confirmPasswordValidation model.confirmPassword)
              }
            , Cmd.none
            )

        BlurConfirmPassword ->
            ( { model
                | confirmPassword =
                    model.confirmPassword
                        |> validate OnBlur (confirmPasswordValidation model.password)
              }
            , Cmd.none
            )

        CheckAcceptPolicy incomingCheck ->
            ( { model | acceptPolicy = field incomingCheck }, Cmd.none )

        Submit ->
            model |> validateModel |> submitIfValid

        SubmitResponse (Ok (Ok ())) ->
            ( { initialModel | status = Succeeded }, Cmd.none )

        SubmitResponse (Ok (Err errs)) ->
            ( { model | status = NotSubmitted }
                |> applyServerValidationErrors errs
            , Cmd.none
            )

        SubmitResponse (Err _) ->
            ( { model | status = Failed }, Cmd.none )

        Cancel ->
            ( { initialModel | status = NotSubmitted }, Cmd.none )


applyServerValidationErrors : List ServerValidationError -> Model -> Model
applyServerValidationErrors errs model =
    let
        applyError err m =
            case err of
                EmailAlreadyRegistered ->
                    { m
                        | email =
                            model.email
                                |> setError "This email is already registered"
                    }
    in
        errs |> List.foldl applyError model


submitIfValid : Model -> ( Model, Cmd Msg )
submitIfValid model =
    let
        submissionResult : Validity (Cmd Msg)
        submissionResult =
            Valid submit
                |: (validity model.email)
                |: (validity model.password)
                |: (validity model.confirmPassword)
                |: (validity model.acceptPolicy)
    in
        case submissionResult of
            Valid cmd ->
                ( { model | status = InProcess }, cmd )

            _ ->
                ( model, Cmd.none )


emailValidation : Validator String String
emailValidation =
    isNotEmpty "An email is required"
        >=> isEmail "Please ensure this is a valid email"


passwordValidation : Validator String String
passwordValidation =
    isNotEmpty "Please enter your password"
        >=> isStrongPassword


confirmPasswordValidation : Field raw String -> Validator String String
confirmPasswordValidation password =
    isNotEmpty "Please retype your password"
        >=> isEqualTo password "The passwords don't match"


validateModel : Model -> Model
validateModel model =
    let
        email =
            model.email |> validate OnSubmit emailValidation

        password =
            model.password
                |> validate OnSubmit passwordValidation

        confirmPassword =
            model.confirmPassword
                |> validate OnSubmit (confirmPasswordValidation password)

        acceptPolicy =
            model.acceptPolicy
                |> validate OnSubmit (isTrue "You must accept the policy")
    in
        { model
            | email = email
            , password = password
            , confirmPassword = confirmPassword
            , acceptPolicy = acceptPolicy
        }


isStrongPassword : String -> Result String String
isStrongPassword p =
    if String.length p >= 5 then
        Ok p
    else
        Err "Your password isn't strong enough"


submit : String -> String -> String -> Bool -> Cmd Msg
submit email password _ _ =
    let
        url =
            "http://localhost:3000/api/register"

        json =
            Encode.object
                [ ( "email", Encode.string email )
                , ( "password", Encode.string password )
                ]

        -- decoder = Decode.string |> Decode.map (always ())
        request : Http.Request ResponseBody
        request =
            Http.post url (Http.jsonBody json) decodeResponseBody
    in
        request |> Http.send SubmitResponse


type alias ResponseBody =
    Result (List ServerValidationError) ()


type ServerValidationError
    = EmailAlreadyRegistered


decodeResponseBody : Decode.Decoder ResponseBody
decodeResponseBody =
    Decode.map2
        (\success errs ->
            if success then
                Ok ()
            else
                Err (errs |> Maybe.withDefault [])
        )
        (Decode.field "success" Decode.bool)
        (Decode.list decodeServerValidationError
            |> Decode.field "errors"
            |> Decode.maybe
        )


decodeServerValidationError : Decode.Decoder ServerValidationError
decodeServerValidationError =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "email_already_registered" ->
                        Decode.succeed EmailAlreadyRegistered

                    s ->
                        Decode.fail <| "Unexpected value: " ++ s
            )


renderHttpRequestStatus : SubmissionStatus -> Html a
renderHttpRequestStatus status =
    case status of
        NotSubmitted ->
            div [] []

        InProcess ->
            div [] [ text "Your request is being sent" ]

        Succeeded ->
            div [] [ text "Your request has been received" ]

        Failed ->
            div [ class "alert alert-danger" ]
                [ text "Ops! There was an error, please try again" ]


errorLabel : Field raw a -> Html Msg
errorLabel field =
    label
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
        , novalidate True
        ]
        [ header model
        , body model
        , footer model

        -- , div [] [ model |> toString |> text ]
        ]


listErrors : Model -> Html msg
listErrors model =
    let
        errors =
            [ extractError model.email
            , extractError model.password
            , extractError model.confirmPassword
            , extractError model.acceptPolicy
            ]
                |> List.concatMap toList

        createListItem s =
            li [] [ text s ]
    in
        case errors of
            [] ->
                text ""

            _ ->
                div []
                    [ text "Please fix the following errors:"
                    , ul [] (errors |> List.map createListItem)
                    ]


header : Model -> Html a
header model =
    div []
        [ h1 [] [ text "Register" ]
        , renderHttpRequestStatus model.status
        , listErrors model
        ]


body : Model -> Html Msg
body model =
    div []
        [ div []
            [ input
                [ placeholder "your email *"
                , type_ "email"
                , onInput InputEmail
                , onBlur BlurEmail

                -- , value (model.email |> displayValue identity)
                , value (model.email |> rawValue)
                , required True
                ]
                []

            -- , errorLabel model.email
            ]
        , div []
            [ input
                [ placeholder "your password *"
                , type_ "password"
                , onInput InputPassword
                , onBlur BlurPassword

                -- , value (model.password |> displayValue identity)
                , value (model.password |> rawValue)
                , required True
                ]
                []

            -- , errorLabel model.password
            ]
        , div []
            [ input
                [ placeholder "confirm password *"
                , type_ "password"
                , onInput InputConfirmPassword
                , onBlur BlurConfirmPassword
                , value (model.confirmPassword |> rawValue)
                , required True
                ]
                []

            -- , errorLabel model.confirmPassword
            ]
        , div []
            [ input
                [ placeholder "your age"
                , type_ "checkbox"
                , onCheck CheckAcceptPolicy

                -- , value (model.age |> displayValue (Maybe.map toString >> Maybe.withDefault ""))
                , value (model.acceptPolicy |> rawValue |> toString)
                ]
                []
            , label [] [ text "I accept the privacy policy" ]
            ]

        -- , div [] [ errorLabel model.acceptPolicy ]
        ]


footer : Model -> Html Msg
footer model =
    div []
        [ button
            [ type_ "submit"
            , disabled (model.status == InProcess)
            ]
            [ text "Submit!" ]
        , button
            [ type_ "button"
            , onClick Cancel
            ]
            [ text "Cancel" ]
        ]
