import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (length)
import Regex exposing (..)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , age : String
  , password : String
  , passwordAgain : String
  }


init : Model
init =
  Model "" "" "" ""



-- UPDATE


type Msg
  = Name String
  | Age String
  | Password String
  | PasswordAgain String
  


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }
    Age age ->
      { model | age = age }
    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "text" "Age" model.age Age
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewValidation model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =

  div []  
  [
    if not(Regex.contains lowerCase model.password) then
       div [ style "color" "red" ] [ text "Password needs at lease one lower case character" ]
    else div [][],
    if not(Regex.contains upperCase model.password) then
       div [ style "color" "red" ] [ text "Password needs at lease one upper case character" ]
       else div [][],
    if not(Regex.contains numeric model.password) then
       div [ style "color" "red" ] [ text "Password needs at lease one numeric character" ]
       else div [][],
    if  Regex.contains nonNumeric model.age  then
      div [ style "color" "red" ] [ text "Age needs to be a number" ]
      else div [][],
    if length model.password < 8 then
    div [ style "color" "red" ] [ text "Password needs to be eight characters or more" ]
    else div [][],
    if  model.password == model.passwordAgain then
    div [ style "color" "green" ] [ text "OK" ]
  else
    div [ style "color" "red" ] [ text "Passwords do not match!" ]
  ]


lowerCase : Regex.Regex
lowerCase = 
  Maybe.withDefault Regex.never <|
    Regex.fromString "[a-z]+"

upperCase : Regex.Regex
upperCase = 
  Maybe.withDefault Regex.never <|
    Regex.fromString "[A-Z]+"

numeric : Regex.Regex
numeric = 
  Maybe.withDefault Regex.never <|
    Regex.fromString "[0-9]+"

nonNumeric : Regex.Regex
nonNumeric = 
  Maybe.withDefault Regex.never <|
    Regex.fromString "[^0-9]+"
