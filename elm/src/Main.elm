module Main exposing (main)

import Html exposing (Html, div, h1, input, p, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)

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
    = GuessChanged String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GuessChanged s ->
            ( { model | guess = s }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Guess it!" ]
        , p [] [ text "Jeu de devinettes en Elm" ]
        , p [] [ text ("Définitions (test) — mot secret : " ++ model.word) ]
        , p [] [ text "Tape ta réponse :" ]
        , input
            [ type_ "text"
            , value model.guess
            , onInput GuessChanged
            ]
            []
        , p [] [ text ("Tu as écrit : " ++ model.guess) ]
        ]

