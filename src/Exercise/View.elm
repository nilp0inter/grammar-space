module Exercise.View exposing (viewExercise)

import Array
import Exercise.Types exposing (..)
import Grammar.Types exposing (VerbTense, verbTenseLabel)
import Html exposing (Html, button, div, p, span, text, textarea)
import Html.Attributes as Attr
import Html.Events as Events


type alias ExerciseConfig msg =
    { oauthLoggedIn : Bool
    , narrativeInput : String
    , exerciseState : Maybe ExerciseState
    , llmLoading : Bool
    , llmError : Maybe String
    , onStartLogin : msg
    , onUpdateNarrative : String -> msg
    , onSubmitNarrative : msg
    , onNextItem : msg
    , onResetExercise : msg
    , timelineView : Html msg
    }


viewExercise : ExerciseConfig msg -> Html msg
viewExercise config =
    div [ Attr.class "flex flex-col items-center gap-6 w-full" ]
        (if not config.oauthLoggedIn then
            [ viewConnectPrompt config.onStartLogin ]

         else
            case config.exerciseState of
                Nothing ->
                    [ viewNarrativeInput config ]

                Just state ->
                    case state.phase of
                        ExerciseComplete ->
                            [ viewScoreSummary state config.onResetExercise ]

                        _ ->
                            viewExerciseActive config state
        )


viewConnectPrompt : msg -> Html msg
viewConnectPrompt onLogin =
    div [ Attr.class "flex flex-col items-center gap-4 py-12" ]
        [ p [ Attr.class "text-slate-400 text-center max-w-md" ]
            [ text "Connect your OpenRouter account to use AI-powered verb tense exercises. You'll need an OpenRouter account with credits — all costs are billed directly to your account." ]
        , button
            [ Attr.class "px-6 py-3 rounded-full bg-indigo-600 text-white font-medium hover:bg-indigo-500 transition-colors"
            , Events.onClick onLogin
            ]
            [ text "Connect with OpenRouter" ]
        ]


viewNarrativeInput : ExerciseConfig msg -> Html msg
viewNarrativeInput config =
    div [ Attr.class "flex flex-col items-center gap-4 w-full" ]
        [ p [ Attr.class "text-slate-400 text-center max-w-lg" ]
            [ text "Write a short narrative in your native language (e.g., Spanish, French, Japanese). The AI will identify the verb tenses used and quiz you on the correct English equivalents." ]
        , textarea
            [ Attr.class "w-full max-w-lg h-32 bg-slate-800 border border-slate-700 rounded-xl px-4 py-3 text-white text-sm resize-none focus:outline-none focus:border-indigo-500"
            , Attr.placeholder "Ayer fui al supermercado. Estaba lloviendo cuando salí de casa. Mañana iré al parque si hace buen tiempo..."
            , Attr.value config.narrativeInput
            , Events.onInput config.onUpdateNarrative
            ]
            []
        , case config.llmError of
            Just err ->
                div [ Attr.class "w-full max-w-lg px-4 py-3 rounded-lg bg-rose-500/10 border border-rose-500/30 text-rose-300 text-sm" ]
                    [ text err ]

            Nothing ->
                text ""
        , button
            [ Attr.class
                (if config.llmLoading || String.isEmpty (String.trim config.narrativeInput) then
                    "px-6 py-3 rounded-full bg-slate-700 text-slate-500 font-medium cursor-not-allowed"

                 else
                    "px-6 py-3 rounded-full bg-indigo-600 text-white font-medium hover:bg-indigo-500 transition-colors"
                )
            , Events.onClick config.onSubmitNarrative
            , Attr.disabled (config.llmLoading || String.isEmpty (String.trim config.narrativeInput))
            ]
            [ if config.llmLoading then
                text "Analyzing..."

              else
                text "Start Exercise"
            ]
        ]


viewExerciseActive : ExerciseConfig msg -> ExerciseState -> List (Html msg)
viewExerciseActive config state =
    let
        maybeItem =
            currentItem state

        progress =
            String.fromInt (state.currentIndex + 1)
                ++ " / "
                ++ String.fromInt (Array.length state.items)
    in
    case maybeItem of
        Just item ->
            [ viewSentenceCard item state.phase progress
            , config.timelineView
            , viewFeedbackArea item state config.onNextItem
            ]

        Nothing ->
            [ text "" ]


viewSentenceCard : ExerciseItem -> ExercisePhase -> String -> Html msg
viewSentenceCard item phase progress =
    div [ Attr.class "flex flex-col items-center gap-3 w-full" ]
        [ span [ Attr.class "text-xs text-slate-500 uppercase tracking-wider" ]
            [ text ("Sentence " ++ progress) ]
        , div [ Attr.class "flex flex-wrap items-center justify-center gap-2 min-h-[4rem] p-6 rounded-xl bg-slate-800/50 w-full" ]
            [ span [ Attr.class "text-lg text-white font-medium text-center" ]
                [ text item.original ]
            ]
        , case phase of
            Answering ->
                p [ Attr.class "text-slate-400 text-sm" ]
                    [ text "Click the correct verb tense on the timeline above" ]

            _ ->
                text ""
        ]


viewFeedbackArea : ExerciseItem -> ExerciseState -> msg -> Html msg
viewFeedbackArea item state onNext =
    case state.phase of
        ShowingFeedback ->
            let
                userAnswer =
                    Array.get state.currentIndex state.answers
                        |> Maybe.andThen identity

                isCorrect =
                    userAnswer == Just item.correctTense
            in
            div [ Attr.class "flex flex-col items-center gap-3 w-full" ]
                [ if isCorrect then
                    div [ Attr.class "px-4 py-3 rounded-lg bg-emerald-500/10 border border-emerald-500/30 text-emerald-300 text-sm text-center w-full" ]
                        [ text ("Correct! " ++ item.explanation) ]

                  else
                    div [ Attr.class "px-4 py-3 rounded-lg bg-rose-500/10 border border-rose-500/30 text-rose-300 text-sm text-center w-full" ]
                        [ text ("The correct answer is " ++ verbTenseLabel item.correctTense ++ ". " ++ item.explanation) ]
                , button
                    [ Attr.class "px-6 py-2 rounded-full bg-indigo-600 text-white font-medium hover:bg-indigo-500 transition-colors"
                    , Events.onClick onNext
                    ]
                    [ text "Next" ]
                ]

        _ ->
            text ""


viewScoreSummary : ExerciseState -> msg -> Html msg
viewScoreSummary state onReset =
    let
        score =
            scoreExercise state

        pct =
            if score.total > 0 then
                toFloat score.correct / toFloat score.total * 100

            else
                0

        colorClass =
            if pct >= 80 then
                "text-emerald-300"

            else if pct >= 50 then
                "text-amber-300"

            else
                "text-rose-300"
    in
    div [ Attr.class "flex flex-col items-center gap-6 py-8" ]
        [ p [ Attr.class "text-slate-400 text-sm uppercase tracking-wider" ]
            [ text "Exercise Complete" ]
        , div [ Attr.class ("text-5xl font-bold " ++ colorClass) ]
            [ text (String.fromInt score.correct ++ " / " ++ String.fromInt score.total) ]
        , p [ Attr.class "text-slate-400" ]
            [ text (String.fromInt (round pct) ++ "% correct") ]
        , button
            [ Attr.class "px-6 py-3 rounded-full bg-indigo-600 text-white font-medium hover:bg-indigo-500 transition-colors"
            , Events.onClick onReset
            ]
            [ text "Try Another" ]
        ]
