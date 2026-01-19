module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, input, label, li, ol, p, span, text, ul)
import Html.Attributes exposing (checked, disabled, name, style, type_, value)
import Html.Events exposing (on, onCheck, onClick, onInput)
import Http
import Json.Decode as Decode
import Random
import Words


-- MAIN


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
    , showWord : Bool
    , revealed : Bool
    , status : Status
    , parts : List Part
    , mode : Mode
    , visibleParts : List Part
    , score : Int
    , bestScore : Int
    , round : Int
    , roundSize : Int
    , turnsLeft : Int
    , feedback : Maybe Feedback
    }


type Status
    = NotAsked
    | Loading
    | Failure String
    | Success


type alias Part =
    { partOfSpeech : String
    , definitions : List String
    }


type Mode
    = Beginner
    | Medium
    | Expert


type Feedback
    = Good String
    | Bad String
    | Info String



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, startRoundCmd initialModel )


initialModel : Model
initialModel =
    { word = ""
    , guess = ""
    , showWord = False
    , revealed = False
    , status = NotAsked
    , parts = []
    , mode = Beginner
    , visibleParts = []
    , score = 0
    , bestScore = 0
    , round = 1
    , roundSize = 10
    , turnsLeft = 10
    , feedback = Nothing
    }



-- MSG


type Msg
    = GuessChanged String
    | SubmitGuess
    | ToggleShow Bool
    | GotRandomIndex Int
    | GotDefinitions (Result Http.Error (List Part))
    | SkipWord
    | NextRound
    | SetMode Mode
    | GotExpertPick Int



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GuessChanged s ->
            ( { model | guess = s, feedback = Nothing }, Cmd.none )

        ToggleShow b ->
            if b then
                ( { model | showWord = True, revealed = True }, Cmd.none )

            else
                ( { model | showWord = False }, Cmd.none )

        SubmitGuess ->
            handleSubmit model

        SkipWord ->
            handleSkip model

        NextRound ->
            startNextRound model

        GotRandomIndex i ->
            handleGotRandomIndex i model

        GotDefinitions result ->
            handleGotDefinitions result model

        GotExpertPick i ->
            ( { model | visibleParts = pickOneDefinition i model.parts }, Cmd.none )

        SetMode m ->
            -- Mode change is allowed only if unlocked
            if isModeUnlocked m model.round then
                let
                    ( visible, cmd ) =
                        setVisibleFromMode m model.parts
                in
                ( { model | mode = m, visibleParts = visible }, cmd )

            else
                ( model, Cmd.none )



-- UPDATE HELPERS (short & clean)


handleSubmit : Model -> ( Model, Cmd Msg )
handleSubmit model =
    if model.turnsLeft <= 0 then
        ( { model | feedback = Just (Info "Round over. Click Next Round!") }, Cmd.none )

    else if String.trim model.guess == "" then
        ( { model | feedback = Just (Info "Type something, then press Enter!") }, Cmd.none )

    else
        let
            correct =
                isCorrect model

            helpUsed =
                model.revealed

            turnsLeft2 =
                model.turnsLeft - 1
        in
        if correct then
            handleCorrect helpUsed turnsLeft2 model

        else
            handleWrong turnsLeft2 model


handleCorrect : Bool -> Int -> Model -> ( Model, Cmd Msg )
handleCorrect helpUsed turnsLeft2 model =
    let
        ( score2, fb ) =
            if helpUsed then
                ( model.score, Good "DID YOU TRY TO CHEAT? You used “show it” so no point !" )

            else
                ( model.score + 1, Good "Right answer! Nice!" )

        m1 =
            model
                |> setScore score2
                |> setTurns turnsLeft2
                |> setFeedback (Just fb)
                |> updateBestScore
                |> resetForNextWord
    in
    if turnsLeft2 == 0 then
        ( { m1 | feedback = Just (Info "🏁 Round finished! Click Next Round!") }, Cmd.none )

    else
        ( m1, randomWordCmd )


handleWrong : Int -> Model -> ( Model, Cmd Msg )
handleWrong turnsLeft2 model =
    let
        m1 =
            model
                |> setTurns turnsLeft2
                |> setFeedback (Just (Bad "Try again!"))
    in
    if turnsLeft2 == 0 then
        ( { m1 | feedback = Just (Info "Round finished! Click Next Round!") }, Cmd.none )

    else
        ( m1, Cmd.none )


handleSkip : Model -> ( Model, Cmd Msg )
handleSkip model =
    if model.turnsLeft <= 0 then
        ( { model | feedback = Just (Info "Round over. Click Next Round!") }, Cmd.none )

    else
        let
            turnsLeft2 =
                model.turnsLeft - 1

            m1 =
                model
                    |> setTurns turnsLeft2
                    |> setFeedback (Just (Bad "Skipped! Counts as wrong."))
                    |> resetForNextWord
        in
        if turnsLeft2 == 0 then
            ( { m1 | feedback = Just (Info "Round finished! Click Next Round!") }, Cmd.none )

        else
            ( m1, randomWordCmd )


startNextRound : Model -> ( Model, Cmd Msg )
startNextRound model =
    if model.round >= 3 then
        ( { model | feedback = Just (Info "Game finished! Great job!") }, Cmd.none )

    else
        let
            newRound =
                model.round + 1

            newMode =
                modeForRound newRound

            m1 =
                { model
                    | round = newRound
                    , mode = newMode
                    , turnsLeft = model.roundSize
                    , feedback = Just (Info ("Round " ++ String.fromInt newRound ++ " unlocked!"))
                }
                    |> resetForNextWord
        in
        ( m1, randomWordCmd )


handleGotRandomIndex : Int -> Model -> ( Model, Cmd Msg )
handleGotRandomIndex i model =
    let
        picked =
            pickWord i
    in
    ( { model
        | word = picked
        , guess = ""
        , showWord = False
        , revealed = False
        , status = Loading
        , parts = []
        , visibleParts = []
      }
    , requestDefinitions picked
    )


handleGotDefinitions : Result Http.Error (List Part) -> Model -> ( Model, Cmd Msg )
handleGotDefinitions result model =
    case result of
        Ok parts ->
            let
                ( visible, cmd ) =
                    setVisibleFromMode model.mode parts
            in
            ( { model | parts = parts, visibleParts = visible, status = Success }, cmd )

        Err _ ->
            ( { model | parts = [], visibleParts = [], status = Failure "No definition found" }
            , Cmd.none
            )



-- PURE MODEL HELPERS


setScore : Int -> Model -> Model
setScore s model =
    { model | score = s }


setTurns : Int -> Model -> Model
setTurns t model =
    { model | turnsLeft = t }


setFeedback : Maybe Feedback -> Model -> Model
setFeedback fb model =
    { model | feedback = fb }


updateBestScore : Model -> Model
updateBestScore model =
    { model | bestScore = max model.bestScore model.score }


resetForNextWord : Model -> Model
resetForNextWord model =
    { model
        | guess = ""
        , showWord = False
        , revealed = False
        , status = Loading
        , parts = []
        , visibleParts = []
      }



-- ROUND / MODE RULES


modeForRound : Int -> Mode
modeForRound r =
    if r <= 1 then
        Beginner

    else if r == 2 then
        Medium

    else
        Expert


isModeUnlocked : Mode -> Int -> Bool
isModeUnlocked m round =
    case ( m, round ) of
        ( Beginner, _ ) ->
            True

        ( Medium, r ) ->
            r >= 2

        ( Expert, r ) ->
            r >= 3



-- COMMANDS


startRoundCmd : Model -> Cmd Msg
startRoundCmd _ =
    randomWordCmd


randomWordCmd : Cmd Msg
randomWordCmd =
    let
        n =
            List.length Words.words
    in
    if n <= 0 then
        Cmd.none
    else
        Random.generate GotRandomIndex (Random.int 0 (n - 1))



-- HTTP


requestDefinitions : String -> Cmd Msg
requestDefinitions word =
    Http.get
        { url = "https://api.dictionaryapi.dev/api/v2/entries/en/" ++ word
        , expect = Http.expectJson GotDefinitions partsDecoder
        }



-- WORD PICKING


pickWord : Int -> String
pickWord i =
    Words.words
        |> List.drop i
        |> List.head
        |> Maybe.withDefault ""



-- DEFINITIONS MODE LOGIC


setVisibleFromMode : Mode -> List Part -> ( List Part, Cmd Msg )
setVisibleFromMode mode parts =
    case mode of
        Beginner ->
            ( parts, Cmd.none )

        Medium ->
            ( pickLargestPart parts, Cmd.none )

        Expert ->
            let
                flat =
                    flatten parts
            in
            if List.isEmpty flat then
                ( [], Cmd.none )
            else
                ( [], Random.generate GotExpertPick (Random.int 0 (List.length flat - 1)) )


pickLargestPart : List Part -> List Part
pickLargestPart parts =
    parts
        |> List.sortBy (\p -> List.length p.definitions)
        |> List.reverse
        |> List.head
        |> Maybe.map (\p -> [ p ])
        |> Maybe.withDefault []


flatten : List Part -> List ( String, String )
flatten parts =
    parts
        |> List.concatMap (\p -> List.map (\d -> ( p.partOfSpeech, d )) p.definitions)


pickOneDefinition : Int -> List Part -> List Part
pickOneDefinition i parts =
    let
        flat =
            flatten parts
    in
    flat
        |> List.drop i
        |> List.head
        |> Maybe.map (\( pos, def ) -> [ { partOfSpeech = pos, definitions = [ def ] } ])
        |> Maybe.withDefault []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "font-family" "system-ui", style "padding" "18px", style "max-width" "720px", style "margin" "0 auto" ]
        [ h1 [ style "margin" "0 0 12px 0" ] [ text (titleText model) ]
        , viewTopBar model
        , viewModeSelector model
        , viewCard
            [ viewStatus model.status
            , viewFeedback model.feedback
            , viewMeaningsBlock model
            , viewGuessInput model
            , viewActions model
            ]
        , viewGameEnd model
        ]


viewCard : List (Html Msg) -> Html Msg
viewCard children =
    div
        [ style "background" "white"
        , style "border-radius" "16px"
        , style "padding" "14px"
        , style "box-shadow" "0 6px 18px rgba(0,0,0,0.08)"
        ]
        children


titleText : Model -> String
titleText model =
    if model.showWord then
        model.word
    else
        "Guess it!"


viewTopBar : Model -> Html Msg
viewTopBar model =
    div [ style "display" "flex", style "gap" "10px", style "flex-wrap" "wrap", style "margin" "8px 0 14px 0" ]
        [ badge "#e7f0ff" ("🟦 Score: " ++ String.fromInt model.score)
        , badge "#f1e7ff" ("🟪 Best: " ++ String.fromInt model.bestScore)
        , badge "#fff3d6" ("🟧 Round " ++ String.fromInt model.round ++ "/3 — Turns: " ++ String.fromInt model.turnsLeft)
        ]


badge : String -> String -> Html Msg
badge bg txt =
    div
        [ style "background" bg
        , style "padding" "8px 10px"
        , style "border-radius" "999px"
        , style "font-weight" "600"
        , style "font-size" "14px"
        ]
        [ text txt ]


viewModeSelector : Model -> Html Msg
viewModeSelector model =
    div [ style "display" "flex", style "gap" "10px", style "margin" "0 0 12px 0", style "align-items" "center" ]
        [ levelChip "Beginner" Beginner True model.mode
        , levelChip "Medium" Medium (isModeUnlocked Medium model.round) model.mode
        , levelChip "Expert" Expert (isModeUnlocked Expert model.round) model.mode
        ]


levelChip : String -> Mode -> Bool -> Mode -> Html Msg
levelChip labelText m unlocked current =
    let
        isCurrent =
            m == current

        bg =
            if unlocked then
                if isCurrent then
                    "#d9ffea"
                else
                    "#f1f5f9"
            else
                "#f3f4f6"

        caption =
            if unlocked then
                labelText
            else
                "🔒 " ++ labelText
    in
    button
        ([ style "border" "0"
         , style "padding" "8px 12px"
         , style "border-radius" "999px"
         , style "cursor" (if unlocked then "pointer" else "not-allowed")
         , style "background" bg
         , style "font-weight" "600"
         ]
            ++ (if unlocked then [ onClick (SetMode m) ] else [])
        )
        [ text caption ]


viewStatus : Status -> Html Msg
viewStatus status =
    case status of
        NotAsked ->
            text ""

        Loading ->
            p [ style "margin" "0 0 8px 0" ] [ text "Loading..." ]

        Success ->
            text ""

        Failure msg ->
            p [ style "margin" "0 0 8px 0" ] [ text ("Error: " ++ msg) ]


viewFeedback : Maybe Feedback -> Html Msg
viewFeedback fb =
    case fb of
        Nothing ->
            text ""

        Just f ->
            let
                ( msg, bg ) =
                    case f of
                        Good s ->
                            ( s, "#e8fff0" )

                        Bad s ->
                            ( s, "#ffecec" )

                        Info s ->
                            ( s, "#eef3ff" )
            in
            div
                [ style "padding" "10px"
                , style "margin" "8px 0"
                , style "border-radius" "12px"
                , style "background" bg
                , style "font-weight" "600"
                ]
                [ text msg ]


viewMeaningsBlock : Model -> Html Msg
viewMeaningsBlock model =
    if model.status == Success then
        if model.mode == Expert && List.isEmpty model.visibleParts then
            p [] [ text "Picking a clue..." ]
        else
            viewMeanings model.visibleParts
    else
        text ""


viewMeanings : List Part -> Html Msg
viewMeanings parts =
    div []
        [ p [ style "margin" "8px 0", style "font-weight" "700" ] [ text "Definitions" ]
        , ul [] (List.map viewPart parts)
        ]


viewGuessInput : Model -> Html Msg
viewGuessInput model =
    div [ style "margin" "10px 0" ]
        [ div [ style "margin" "0 0 6px 0" ] [ text "Type your guess, then press Enter" ]
        , input
            [ type_ "text"
            , value model.guess
            , onInput GuessChanged
            , onEnter SubmitGuess
            , disabled (model.turnsLeft <= 0)
            , style "width" "100%"
            , style "padding" "10px"
            , style "border-radius" "10px"
            , style "border" "1px solid #e5e7eb"
            ]
            []
        ]


viewActions : Model -> Html Msg
viewActions model =
    div [ style "display" "flex", style "gap" "10px", style "align-items" "center", style "flex-wrap" "wrap" ]
        [ button
            [ onClick SkipWord
            , disabled (model.turnsLeft <= 0)
            , style "border" "0"
            , style "padding" "10px 12px"
            , style "border-radius" "12px"
            , style "background" "#ffecec"
            , style "font-weight" "700"
            ]
            [ text "Skip (counts wrong)" ]
        , viewShowIt model
        , if model.turnsLeft == 0 && model.round < 3 then
            button
                [ onClick NextRound
                , style "border" "0"
                , style "padding" "10px 12px"
                , style "border-radius" "12px"
                , style "background" "#eef3ff"
                , style "font-weight" "800"
                ]
                [ text "Next Round" ]
          else
            text ""
        ]


viewShowIt : Model -> Html Msg
viewShowIt model =
    label [ style "display" "flex", style "gap" "8px", style "align-items" "center" ]
        [ input
            [ type_ "checkbox"
            , checked model.showWord
            , onCheck ToggleShow
            , disabled (model.turnsLeft <= 0)
            ]
            []
        , text "Show it"
        ]


viewGameEnd : Model -> Html Msg
viewGameEnd model =
    if model.round == 3 && model.turnsLeft == 0 then
        div [ style "margin" "14px 0", style "padding" "12px", style "border-radius" "14px", style "background" "#f1e7ff", style "font-weight" "800" ]
            [ text ("🎉 Game finished! Final score: " ++ String.fromInt model.score ++ " — Best: " ++ String.fromInt model.bestScore) ]

    else
        text ""


viewPart : Part -> Html Msg
viewPart part =
    li [ style "margin" "10px 0" ]
        [ p [ style "margin" "0 0 6px 0", style "font-weight" "700" ] [ text part.partOfSpeech ]
        , ol [] (List.map (\d -> li [] [ text d ]) part.definitions)
        ]



-- ENTER KEY HANDLER


onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    on "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    if key == "Enter" then
                        Decode.succeed msg
                    else
                        Decode.fail "not enter"
                )
        )



-- GUESS CHECK


isCorrect : Model -> Bool
isCorrect model =
    let
        g =
            normalize model.guess

        w =
            normalize model.word
    in
    g /= "" && g == w


normalize : String -> String
normalize s =
    s |> String.trim |> String.toLower



-- JSON DECODERS


partsDecoder : Decode.Decoder (List Part)
partsDecoder =
    Decode.list entryDecoder
        |> Decode.map (\entries -> entries |> List.head |> Maybe.withDefault [])


entryDecoder : Decode.Decoder (List Part)
entryDecoder =
    Decode.field "meanings" (Decode.list meaningDecoder)


meaningDecoder : Decode.Decoder Part
meaningDecoder =
    Decode.map2 Part
        (Decode.field "partOfSpeech" Decode.string)
        (Decode.field "definitions" (Decode.list definitionDecoder))


definitionDecoder : Decode.Decoder String
definitionDecoder =
    Decode.field "definition" Decode.string

