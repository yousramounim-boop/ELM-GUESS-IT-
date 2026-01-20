module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, input, label, li, ol, p, span, text, ul)
import Html.Attributes exposing (checked, style, type_, value)
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
    , lives : Int
    , feedback : Maybe Feedback
    , hasTried : Bool

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
    , lives = 5 
    , feedback = Nothing
    , hasTried = False
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
    | Replay

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
            if isGameFinished model then
                ( model, Cmd.none )
            else
                handleSubmit model

        SkipWord ->
            if isGameFinished model then 
                ( model, Cmd.none )
            else
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
            if isModeUnlocked m model.score then
                let
                    -- calculer les parties visibles selon le mode choisi
                    ( visible, cmd ) =
                        setVisibleFromMode m model.parts

                    -- mettre à jour le modèle avec le nouveau mode
                    m1 =
                        { model | mode = m, visibleParts = visible }
                in
                -- passer directement au prochain mot si le mode est débloqué
                startNextRound m1
            else
                ( model, Cmd.none )

        Replay ->
            -- réinitialiser le jeu mais conserver le meilleur score
            let
                m1 =
                    { initialModel
                        | bestScore = model.bestScore
                        , lives = 5
                        , score = 0
                        , feedback = Nothing
                        , hasTried = False
                    }
            in
            ( m1, startRoundCmd m1 )




-- UPDATE HELPERS

handleSubmit : Model -> ( Model, Cmd Msg )
handleSubmit model =
    -- bloqué si game over ou victoire
    if isGameFinished model then
        ( model, Cmd.none )

    else if String.trim model.guess == "" then
        ( { model | feedback = Just (Info "Type something, then press Enter!") }, Cmd.none )

    else
        let
            correct = isCorrect model
            helpUsed = model.revealed
            hasTriedNow = True
        in
        if correct then
            handleCorrect helpUsed model
        else
            -- mauvaise réponse
            let
                m1 =
                    { model
                        | lives = model.lives - 1
                        , feedback = Just (Bad "Try again!")
                        , hasTried = hasTriedNow
                    }
            in
            ( m1, Cmd.none )



-- HANDLE CORRECT (score-based)


handleCorrect : Bool -> Model -> ( Model, Cmd Msg )
handleCorrect helpUsed model =
    let
        -- calcul du nouveau score et vies
        ( score2, lives2, fb ) =
            if helpUsed then
                ( model.score
                , model.lives - 1
                , Good "DID YOU TRY TO CHEAT? You lost a life, FOCUS!"
                )
            else
                ( model.score + 1
                , model.lives
                , Good "Right answer! Nice!"
                )

        -- passage de niveau immédiat selon score
        newMode =
            modeFromScore score2

        -- mise à jour visibleParts selon le nouveau mode après une bonne réponse 
        (visibleParts2, _) =
            setVisibleFromMode newMode model.parts

        -- modèle mis à jour
        m1 =
            { model
                | score = score2
                , lives = lives2
                , feedback = Just fb
                , mode = newMode
                , visibleParts = visibleParts2
                , hasTried = False
            }
            |> updateBestScore 
    in
    if score2 >= 20 then
        -- victoire immédiate
        ( { m1 | feedback = Just (Info "🎉 Victory! You reached 20 points!"), lives = 0 }
        , Cmd.none
        )
    else
        -- continuer normalement avec mot suivant
        ( resetForNextWord m1, randomWordCmd )


handleSkip : Model -> ( Model, Cmd Msg )
handleSkip model =
    -- si Game Over ou victoire → bloquer skip
    if model.lives <= 0 then
        ( { model | feedback = Just (Info "Game Over! Cannot skip.") }, Cmd.none )

    else if model.score >= 20 then
        ( { model | feedback = Just (Info "You already won, congrats!") }, Cmd.none )

    else
        let
            newLives =
                if model.hasTried then
                    model.lives        -- si déjà tenté, pas de cœur en moins
                else
                    model.lives - 1    -- sinon on enlève un cœur
            m1 =
                model
                    |> setLives newLives
                    |> setFeedback (Just (Bad "Skipped!"))
                    |> resetForNextWord
        in
        ( m1, randomWordCmd )


setLives : Int -> Model -> Model
setLives l model =
    { model | lives = max 0 l }  -- pas de vies négatives


-- START NEXT ROUND (score-based)

startNextRound : Model -> ( Model, Cmd Msg )
startNextRound model =
    if model.lives <= 0 then
        ( { model | feedback = Just (Info "You have no lives left! Game Over!") }, Cmd.none )

    else if model.score >= 20 then
        ( { model | feedback = Just (Info ("You won! Final score: " ++ String.fromInt model.score)) }, Cmd.none )

    else
        ( resetForNextWord model, randomWordCmd )


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
            -- Tirer automatiquement un nouveau mot
            ( model, randomWordCmd )



-- PURE MODEL HELPERS


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
        , hasTried = False
      }

isGameFinished : Model -> Bool
isGameFinished model =
    model.lives <= 0 || model.score >= 20

-- ROUND / MODE RULES pour les scores

isModeUnlocked : Mode -> Int -> Bool
isModeUnlocked m score =
    case m of
        Beginner ->
            score < 5  -- facile accessible seulement avant score 5
        Medium ->
            score >= 5 && score < 10  -- moyen accessible entre 5 et 9
        Expert  -> 
            score >= 10  -- Expert débloqué entre 10 et 19

modeFromScore : Int -> Mode
modeFromScore score =
    if score < 5 then
        Beginner
    else if score < 10 then
        Medium
    else
        Expert


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

-- Détermine quelles définitions sont visibles selon le mode : Beginner/Medium/Expert
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
        (if isGameFinished model then 
            [ viewGameEnd model ]
         else
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
            ]
        )


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
    div [ style "display" "flex", style "gap" "10px", style "flex-wrap" "wrap", style "margin" "8px 0 14px 0", style "justify-content" "space-between" ]
        [ div []
            [ badge "#e7f0ff" ("🟦 Score: " ++ String.fromInt model.score)
            , badge "#f1e7ff" ("🟪 Best: " ++ String.fromInt model.bestScore)
          ]
        , div [] (List.map (\_ -> text "❤️") (List.range 1 model.lives))
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


-- VIEW MODE SELECTOR (score-based)

viewModeSelector : Model -> Html Msg
viewModeSelector model =
    div [ style "display" "flex", style "gap" "10px", style "margin" "0 0 12px 0", style "align-items" "center" ]
        [ levelChip "Beginner" Beginner (isModeUnlocked Beginner model.score) model.mode
        , levelChip "Medium" Medium (isModeUnlocked Medium model.score) model.mode
        , levelChip "Expert" Expert (isModeUnlocked Expert model.score) model.mode
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
            , style "border" "0"
            , style "padding" "10px 12px"
            , style "border-radius" "12px"
            , style "background" "#ffecec"
            , style "font-weight" "700"
            ]
            [ text "Skip" ]
        , viewShowIt model
                ]


viewShowIt : Model -> Html Msg
viewShowIt model =
    label [ style "display" "flex", style "gap" "8px", style "align-items" "center" ]
        [ input
            [ type_ "checkbox"
            , checked model.showWord
            , onCheck ToggleShow
            ]
            []
        , text "Show it"
        ]


viewGameEnd : Model -> Html Msg
viewGameEnd model =
    if model.score >= 20 then
        -- VICTOIRE à 20 points
        div
            [ style "margin" "40px 0"
            , style "padding" "30px"
            , style "border-radius" "16px"
            , style "background" "#d9ffea"
            , style "text-align" "center"
            , style "font-size" "36px"
            , style "font-weight" "900"
            , style "color" "#0a7f3f"
            ]
            [ text "CONGRATULATIONS!"
            , div [ style "margin-top" "20px" ]
                [ button
                    [ onClick Replay
                    , style "padding" "12px 20px"
                    , style "border-radius" "12px"
                    , style "border" "0"
                    , style "background" "#f1e7ff"
                    , style "font-weight" "700"
                    , style "cursor" "pointer"
                    , style "font-size" "18px"
                    ]
                    [ text "Replay" ]
                ]
            ]

    else if model.lives <= 0 then
        -- GAME OVER
        div 
            [ style "margin" "40px 0"
            , style "padding" "30px"
            , style "border-radius" "16px"
            , style "background" "#ffecec"
            , style "text-align" "center"
            , style "font-size" "28px"
            , style "font-weight" "800"
            , style "color" "#a10f0f"
            ]
            [ text ("Game over! Final score: " ++ String.fromInt model.score ++ " — Best: " ++ String.fromInt model.bestScore)
            , div [ style "margin-top" "20px" ]
                [ button 
                    [ onClick Replay
                    , style "padding" "12px 20px"
                    , style "border-radius" "12px"
                    , style "border" "0"
                    , style "background" "#d9ffea"
                    , style "font-weight" "700"
                    , style "cursor" "pointer"
                    , style "font-size" "18px"
                    ]
                    [ text "Replay" ]
                ]
            ]

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