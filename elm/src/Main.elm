module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, p, text)
import Html.Events exposing (onClick)

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
-- MODEL
type alias Model =
    { word : String
    , guess : String
    }

    -- INIT
init : () -> ( Model, Cmd Msg )
init _ =
    ( { word = "hardly", guess = "" }, Cmd.none )
    
type Msg
  = Increment
  | Decrement

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )

view model =
    div []
        [ h1 [] [ text "Guess it!" ]
        , p [] [ text "Jeu de devinettes en Elm" ]
        , button [ onClick Decrement ] [ text "-" ]
        , p [] [ text ("Mot secret (test) : " ++ model.word) ]
        , button [ onClick Increment ] [ text "+" ]
        ]
