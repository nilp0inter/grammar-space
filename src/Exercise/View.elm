module Exercise.View exposing (viewExercise)

import Array
import Exercise.Types exposing (..)
import Grammar.Types exposing (Aspect(..), Tense(..), VerbTense, allAspects, allTenses, aspectLabel, tenseLabel, verbTenseForCell, verbTenseLabel, verbTensesForAspect, verbTensesForTense)
import Html exposing (Html, button, div, label, option, p, select, span, text, textarea)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode
import Prompt
import Story exposing (SavedStory, StoryMode(..))


type alias ExerciseConfig msg =
    { oauthLoggedIn : Bool
    , narrativeInput : String
    , exerciseState : Maybe ExerciseState
    , llmLoading : Bool
    , llmError : Maybe String
    , availableModels : List { id : String, name : String }
    , selectedModelId : String
    , onSelectModel : String -> msg
    , onStartLogin : msg
    , onUpdateNarrative : String -> msg
    , onSubmitNarrative : msg
    , onNextItem : msg
    , onPrevItem : msg
    , onAcceptTense : msg
    , onFinishExercise : msg
    , onResetExercise : msg
    , timelineView : Html msg
    , exerciseInputMode : ExerciseInputMode
    , selectedLanguage : String
    , storyLanguages : List { code : String, name : String }
    , onSetExerciseInputMode : ExerciseInputMode -> msg
    , onSelectLanguage : String -> msg
    , onSubmitGenerateStory : msg
    , savedStories : List SavedStory
    , onLoadSavedStory : SavedStory -> msg
    , onDeleteSavedStory : String -> msg
    , exerciseTypeChoice : ExerciseTypeChoice
    , onChooseExerciseType : ExerciseTypeChoice -> msg
    , onUpdateTranslationInput : String -> msg
    , onSubmitTranslation : msg
    , selectedTopic : StoryTopic
    , customTopicInput : String
    , selectedTenses : List VerbTense
    , onSelectStoryTopic : StoryTopic -> msg
    , onUpdateCustomTopic : String -> msg
    , onToggleStoryTense : VerbTense -> msg
    , onToggleTenseColumn : Tense -> msg
    , onToggleTenseRow : Aspect -> msg
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
        [ viewInputModeToggle config
        , viewExerciseTypeToggle config
        , case config.exerciseInputMode of
            WriteOwnMode ->
                viewWriteOwnInput config

            GenerateStoryMode ->
                viewGenerateStoryInput config

            SavedStoriesMode ->
                viewSavedStoriesList config
        , case config.exerciseInputMode of
            SavedStoriesMode ->
                text ""

            _ ->
                div [ Attr.class "flex flex-col items-center gap-4 w-full" ]
                    [ viewModelSelector config
                    , case config.llmError of
                        Just err ->
                            div [ Attr.class "w-full max-w-lg px-4 py-3 rounded-lg bg-rose-500/10 border border-rose-500/30 text-rose-300 text-sm" ]
                                [ text err ]

                        Nothing ->
                            text ""
                    , viewSubmitButton config
                    ]
        ]


viewInputModeToggle : ExerciseConfig msg -> Html msg
viewInputModeToggle config =
    div [ Attr.class "inline-flex rounded-full bg-slate-800/50 p-0.5" ]
        [ viewModeTab (config.exerciseInputMode == WriteOwnMode) (config.onSetExerciseInputMode WriteOwnMode) "Write Your Own"
        , viewModeTab (config.exerciseInputMode == GenerateStoryMode) (config.onSetExerciseInputMode GenerateStoryMode) "Generate Story"
        , viewModeTab (config.exerciseInputMode == SavedStoriesMode) (config.onSetExerciseInputMode SavedStoriesMode) "Saved Stories"
        ]


viewExerciseTypeToggle : ExerciseConfig msg -> Html msg
viewExerciseTypeToggle config =
    div [ Attr.class "inline-flex rounded-full bg-slate-800/50 p-0.5" ]
        [ viewModeTab (config.exerciseTypeChoice == TenseIdChoice) (config.onChooseExerciseType TenseIdChoice) "Identify Tense"
        , viewModeTab (config.exerciseTypeChoice == TranslationChoice) (config.onChooseExerciseType TranslationChoice) "Translate"
        ]


viewModeTab : Bool -> msg -> String -> Html msg
viewModeTab isActive msg labelText =
    let
        baseClass =
            "px-5 py-2 rounded-full text-sm font-medium transition-colors cursor-pointer select-none"

        colorClass =
            if isActive then
                "bg-indigo-600 text-white"

            else
                "text-slate-400 hover:text-slate-200"
    in
    span
        [ Attr.class (baseClass ++ " " ++ colorClass)
        , Events.onClick msg
        ]
        [ text labelText ]


viewWriteOwnInput : ExerciseConfig msg -> Html msg
viewWriteOwnInput config =
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
        ]


viewGenerateStoryInput : ExerciseConfig msg -> Html msg
viewGenerateStoryInput config =
    div [ Attr.class "flex flex-col items-center gap-4 w-full" ]
        [ p [ Attr.class "text-slate-400 text-center max-w-lg" ]
            [ text "Choose a topic and tenses, then the AI will generate a short story in your chosen language." ]
        , div [ Attr.class "flex flex-col gap-1 w-full max-w-lg" ]
            [ label [ Attr.class "text-xs font-medium text-slate-500 uppercase tracking-wider" ]
                [ text "Story Language" ]
            , select
                [ Attr.class "w-full bg-slate-800 border border-slate-700 rounded-xl px-4 py-3 text-white text-sm focus:outline-none focus:border-indigo-500 appearance-none"
                , Attr.value config.selectedLanguage
                , Events.onInput config.onSelectLanguage
                ]
                (List.map
                    (\lang ->
                        option
                            [ Attr.value lang.code
                            , Attr.selected (lang.code == config.selectedLanguage)
                            ]
                            [ text lang.name ]
                    )
                    config.storyLanguages
                )
            ]
        , viewTopicSelector config
        , viewTenseGrid config
        ]


viewSubmitButton : ExerciseConfig msg -> Html msg
viewSubmitButton config =
    case config.exerciseInputMode of
        WriteOwnMode ->
            let
                isDisabled =
                    config.llmLoading || String.isEmpty (String.trim config.narrativeInput)
            in
            button
                [ Attr.class
                    (if isDisabled then
                        "px-6 py-3 rounded-full bg-slate-700 text-slate-500 font-medium cursor-not-allowed"

                     else
                        "px-6 py-3 rounded-full bg-indigo-600 text-white font-medium hover:bg-indigo-500 transition-colors"
                    )
                , Events.onClick config.onSubmitNarrative
                , Attr.disabled isDisabled
                ]
                [ if config.llmLoading then
                    text "Analyzing..."

                  else
                    text "Start Exercise"
                ]

        GenerateStoryMode ->
            let
                tooFewTenses =
                    List.length config.selectedTenses < 2

                isDisabled =
                    config.llmLoading || tooFewTenses
            in
            button
                [ Attr.class
                    (if isDisabled then
                        "px-6 py-3 rounded-full bg-slate-700 text-slate-500 font-medium cursor-not-allowed"

                     else
                        "px-6 py-3 rounded-full bg-indigo-600 text-white font-medium hover:bg-indigo-500 transition-colors"
                    )
                , Events.onClick config.onSubmitGenerateStory
                , Attr.disabled isDisabled
                ]
                [ if config.llmLoading then
                    text "Generating..."

                  else
                    text "Generate & Start"
                ]

        SavedStoriesMode ->
            text ""


viewModelSelector : ExerciseConfig msg -> Html msg
viewModelSelector config =
    let
        models =
            if List.isEmpty config.availableModels then
                [ { id = config.selectedModelId, name = config.selectedModelId } ]

            else
                config.availableModels
    in
    div [ Attr.class "flex flex-col gap-1 w-full max-w-lg" ]
        [ label [ Attr.class "text-xs font-medium text-slate-500 uppercase tracking-wider" ]
            [ text "Model" ]
        , select
            [ Attr.class "w-full bg-slate-800 border border-slate-700 rounded-xl px-4 py-3 text-white text-sm focus:outline-none focus:border-indigo-500 appearance-none"
            , Attr.value config.selectedModelId
            , Events.onInput config.onSelectModel
            ]
            (List.map
                (\m ->
                    option
                        [ Attr.value m.id
                        , Attr.selected (m.id == config.selectedModelId)
                        ]
                        [ text m.name ]
                )
                models
            )
        ]



-- =========================================================
-- Topic Selector
-- =========================================================


viewTopicSelector : ExerciseConfig msg -> Html msg
viewTopicSelector config =
    let
        currentTopicId =
            case config.selectedTopic of
                PredefinedTopic tid ->
                    tid

                CustomTopic ->
                    "__custom__"

        onTopicChange val =
            if val == "__custom__" then
                config.onSelectStoryTopic CustomTopic

            else
                config.onSelectStoryTopic (PredefinedTopic val)
    in
    div [ Attr.class "flex flex-col gap-1 w-full max-w-lg" ]
        ([ label [ Attr.class "text-xs font-medium text-slate-500 uppercase tracking-wider" ]
            [ text "Topic" ]
         , select
            [ Attr.class "w-full bg-slate-800 border border-slate-700 rounded-xl px-4 py-3 text-white text-sm focus:outline-none focus:border-indigo-500 appearance-none"
            , Attr.value currentTopicId
            , Events.onInput onTopicChange
            ]
            (List.map
                (\t ->
                    option
                        [ Attr.value t.id
                        , Attr.selected (currentTopicId == t.id)
                        ]
                        [ text t.label ]
                )
                storyTopics
                ++ [ option
                        [ Attr.value "__custom__"
                        , Attr.selected (currentTopicId == "__custom__")
                        ]
                        [ text "Custom..." ]
                   ]
            )
         ]
            ++ (case config.selectedTopic of
                    CustomTopic ->
                        [ Html.input
                            [ Attr.class "w-full bg-slate-800 border border-slate-700 rounded-xl px-4 py-3 text-white text-sm focus:outline-none focus:border-indigo-500 mt-2"
                            , Attr.placeholder "Enter your custom topic..."
                            , Attr.value config.customTopicInput
                            , Events.onInput config.onUpdateCustomTopic
                            ]
                            []
                        ]

                    PredefinedTopic _ ->
                        []
               )
        )



-- =========================================================
-- Tense Grid
-- =========================================================


viewTenseGrid : ExerciseConfig msg -> Html msg
viewTenseGrid config =
    let
        selectedCount =
            List.length config.selectedTenses

        countText =
            String.fromInt selectedCount ++ " of 12 tenses selected"

        countClass =
            if selectedCount < 2 then
                "text-rose-400 text-xs"

            else
                "text-slate-500 text-xs"
    in
    div [ Attr.class "flex flex-col gap-2 w-full max-w-lg" ]
        [ div [ Attr.class "flex items-center justify-between" ]
            [ label [ Attr.class "text-xs font-medium text-slate-500 uppercase tracking-wider" ]
                [ text "Verb Tenses" ]
            , span [ Attr.class countClass ]
                [ text
                    (if selectedCount < 2 then
                        countText ++ " (minimum 2)"

                     else
                        countText
                    )
                ]
            ]
        , div [ Attr.class "grid grid-cols-4 gap-1" ]
            ([ -- Top-left empty corner
               div [ Attr.class "p-1" ] []
             ]
                ++ -- Column headers (Past, Present, Future)
                   List.map
                    (\t ->
                        let
                            colTenses =
                                verbTensesForTense t

                            allSelected =
                                List.all (\vt -> List.member vt config.selectedTenses) colTenses
                        in
                        div
                            [ Attr.class
                                ("p-1.5 text-center text-xs font-semibold rounded cursor-pointer select-none transition-colors "
                                    ++ (if allSelected then
                                            "text-indigo-300 bg-indigo-500/20"

                                        else
                                            "text-slate-400 hover:text-slate-200"
                                       )
                                )
                            , Events.onClick (config.onToggleTenseColumn t)
                            ]
                            [ text (tenseLabel t) ]
                    )
                    allTenses
                ++ -- Rows: one per aspect
                   List.concatMap
                    (\a ->
                        let
                            rowTenses =
                                verbTensesForAspect a

                            allRowSelected =
                                List.all (\vt -> List.member vt config.selectedTenses) rowTenses
                        in
                        -- Row header
                        [ div
                            [ Attr.class
                                ("p-1.5 text-xs font-semibold rounded cursor-pointer select-none transition-colors "
                                    ++ (if allRowSelected then
                                            "text-indigo-300 bg-indigo-500/20"

                                        else
                                            "text-slate-400 hover:text-slate-200"
                                       )
                                )
                            , Events.onClick (config.onToggleTenseRow a)
                            ]
                            [ text (aspectLabel a) ]
                        ]
                            ++ -- Cells for this row
                               List.map
                                (\t ->
                                    let
                                        vt =
                                            verbTenseForCell t a

                                        isSelected =
                                            List.member vt config.selectedTenses
                                    in
                                    div
                                        [ Attr.class
                                            ("p-1.5 text-center text-xs rounded cursor-pointer select-none transition-colors "
                                                ++ (if isSelected then
                                                        "bg-indigo-600 text-white"

                                                    else
                                                        "bg-slate-800/50 text-slate-600 hover:text-slate-400"
                                                   )
                                            )
                                        , Events.onClick (config.onToggleStoryTense vt)
                                        ]
                                        [ text (verbTenseLabel vt) ]
                                )
                                allTenses
                    )
                    allAspects
            )
        ]



-- =========================================================
-- Saved Stories List
-- =========================================================


viewSavedStoriesList : ExerciseConfig msg -> Html msg
viewSavedStoriesList config =
    if List.isEmpty config.savedStories then
        div [ Attr.class "flex flex-col items-center gap-3 py-8" ]
            [ p [ Attr.class "text-slate-500 text-sm" ]
                [ text "No saved stories yet. Complete an exercise to save it." ]
            ]

    else
        div [ Attr.class "flex flex-col gap-3 w-full max-w-lg" ]
            (List.map (viewStoryCard config) config.savedStories)


viewStoryCard : ExerciseConfig msg -> SavedStory -> Html msg
viewStoryCard config story =
    let
        langName =
            config.storyLanguages
                |> List.filter (\l -> l.code == story.language)
                |> List.head
                |> Maybe.map .name
                |> Maybe.withDefault story.language

        sentenceCount =
            List.length story.items

        modeLabel =
            case story.mode of
                UserWritten _ ->
                    "Written"

                AIGenerated ->
                    "Generated"
    in
    div [ Attr.class "flex items-center gap-3 p-4 rounded-xl bg-slate-800/50 border border-slate-700/50" ]
        [ div [ Attr.class "flex-1 min-w-0" ]
            [ div [ Attr.class "flex items-center gap-2 mb-1" ]
                [ span [ Attr.class "text-white text-sm font-medium truncate" ]
                    [ text story.title ]
                ]
            , div [ Attr.class "flex items-center gap-2" ]
                [ span [ Attr.class "px-2 py-0.5 rounded-full bg-indigo-500/20 text-indigo-300 text-xs" ]
                    [ text langName ]
                , span [ Attr.class "px-2 py-0.5 rounded-full bg-slate-700 text-slate-400 text-xs" ]
                    [ text modeLabel ]
                , span [ Attr.class "text-slate-500 text-xs" ]
                    [ text (String.fromInt sentenceCount ++ " sentences") ]
                ]
            ]
        , button
            [ Attr.class "px-4 py-2 rounded-full bg-indigo-600 text-white text-sm font-medium hover:bg-indigo-500 transition-colors"
            , Events.onClick (config.onLoadSavedStory story)
            ]
            [ text "Replay" ]
        , button
            [ Attr.class "px-3 py-2 rounded-full text-slate-500 hover:text-rose-400 text-sm transition-colors"
            , Events.onClick (config.onDeleteSavedStory story.id)
            ]
            [ text "Delete" ]
        ]



-- =========================================================
-- Active Exercise
-- =========================================================


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
            case state.kind of
                TenseIdentification ti ->
                    [ viewSentenceCard item state.phase progress "Click a verb tense on the timeline"
                    , config.timelineView
                    , viewTenseIdInteraction item state ti config.onAcceptTense
                    , viewTenseIdReadonlyFeedback item state ti
                    , viewNavigationBar config state
                    ]

                Translation ts ->
                    [ viewTranslationSentenceCard item progress
                    , config.timelineView
                    , viewTranslationInteraction config item state ts
                    , viewTranslationReadonlyFeedback ts state.currentIndex
                    , viewNavigationBar config state
                    ]

        Nothing ->
            [ text "" ]


viewSentenceCard : ExerciseItem -> ExercisePhase -> String -> String -> Html msg
viewSentenceCard item phase progress instruction =
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
                    [ text instruction ]

            _ ->
                text ""
        ]


viewTranslationSentenceCard : ExerciseItem -> String -> Html msg
viewTranslationSentenceCard item progress =
    div [ Attr.class "flex flex-col items-center gap-3 w-full" ]
        [ span [ Attr.class "text-xs text-slate-500 uppercase tracking-wider" ]
            [ text ("Sentence " ++ progress) ]
        , div [ Attr.class "flex flex-wrap items-center justify-center gap-2 min-h-[4rem] p-6 rounded-xl bg-slate-800/50 w-full" ]
            [ span [ Attr.class "text-lg text-white font-medium text-center" ]
                (Prompt.viewPrompt item.prompt)
            ]
        , p [ Attr.class "text-slate-400 text-sm" ]
            [ text "Translate this sentence into English (check the timeline for the correct tense)" ]
        ]


onEnter : msg -> Html.Attribute msg
onEnter msg =
    Events.on "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    if key == "Enter" then
                        Decode.succeed msg

                    else
                        Decode.fail "not enter"
                )
        )


viewTenseIdInteraction : ExerciseItem -> ExerciseState -> { answers : Array.Array (Maybe VerbTense), selectedTense : Maybe VerbTense } -> msg -> Html msg
viewTenseIdInteraction item state ti onAccept =
    case state.phase of
        Answering ->
            case ti.selectedTense of
                Just selectedVt ->
                    div [ Attr.class "flex justify-center w-full" ]
                        [ button
                            [ Attr.class "px-6 py-2 rounded-full bg-emerald-600 text-white font-medium hover:bg-emerald-500 transition-colors"
                            , Events.onClick onAccept
                            ]
                            [ text ("Accept: " ++ verbTenseLabel selectedVt) ]
                        ]

                Nothing ->
                    text ""

        _ ->
            text ""


viewTenseIdReadonlyFeedback : ExerciseItem -> ExerciseState -> { answers : Array.Array (Maybe VerbTense), selectedTense : Maybe VerbTense } -> Html msg
viewTenseIdReadonlyFeedback item state ti =
    case state.phase of
        ShowingFeedback ->
            let
                userAnswer =
                    Array.get state.currentIndex ti.answers
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
                ]

        _ ->
            text ""


viewTranslationInteraction : ExerciseConfig msg -> ExerciseItem -> ExerciseState -> TranslationState -> Html msg
viewTranslationInteraction config item state ts =
    let
        currentInput =
            Array.get state.currentIndex ts.inputs
                |> Maybe.withDefault ""

        currentEval =
            Array.get state.currentIndex ts.evaluations
                |> Maybe.andThen identity

        hasEvaluation =
            currentEval /= Nothing

        isDisabled =
            ts.evaluating || String.isEmpty (String.trim currentInput) || hasEvaluation
    in
    div [ Attr.class "flex flex-col items-center gap-3 w-full" ]
        [ Html.input
            [ Attr.id "translation-input"
            , Attr.class "w-full max-w-lg bg-slate-800 border border-slate-700 rounded-xl px-4 py-3 text-white text-sm focus:outline-none focus:border-indigo-500"
            , Attr.placeholder "Type your English translation..."
            , Attr.value currentInput
            , Attr.disabled hasEvaluation
            , Events.onInput config.onUpdateTranslationInput
            , if not hasEvaluation && not (String.isEmpty (String.trim currentInput)) && not ts.evaluating then
                onEnter config.onSubmitTranslation

              else
                Attr.class ""
            ]
            []
        , if hasEvaluation then
            text ""

          else
            button
                [ Attr.class
                    (if isDisabled then
                        "px-6 py-2 rounded-full bg-slate-700 text-slate-500 font-medium cursor-not-allowed"

                     else
                        "px-6 py-2 rounded-full bg-indigo-600 text-white font-medium hover:bg-indigo-500 transition-colors"
                    )
                , Attr.disabled isDisabled
                , Events.onClick config.onSubmitTranslation
                ]
                [ if ts.evaluating then
                    text "Evaluating..."

                  else
                    text "Submit Translation"
                ]
        ]


viewTranslationReadonlyFeedback : TranslationState -> Int -> Html msg
viewTranslationReadonlyFeedback ts currentIndex =
    let
        currentEval =
            Array.get currentIndex ts.evaluations
                |> Maybe.andThen identity
    in
    case currentEval of
        Just eval ->
            let
                feedbackStyle =
                    case eval of
                        Perfect _ ->
                            { bg = "bg-emerald-500/10 border-emerald-500/30", txt = "text-emerald-300", label = "Perfect!" }

                        Good _ ->
                            { bg = "bg-amber-500/10 border-amber-500/30", txt = "text-amber-300", label = "Good" }

                        Wrong _ ->
                            { bg = "bg-rose-500/10 border-rose-500/30", txt = "text-rose-300", label = "Wrong" }

                explanation =
                    case eval of
                        Perfect expl ->
                            expl

                        Good expl ->
                            expl

                        Wrong expl ->
                            expl
            in
            div [ Attr.class "flex flex-col items-center gap-3 w-full" ]
                [ div [ Attr.class ("px-4 py-3 rounded-lg border text-sm text-center w-full " ++ feedbackStyle.bg ++ " " ++ feedbackStyle.txt) ]
                    [ span [ Attr.class "font-semibold" ] [ text feedbackStyle.label ]
                    , text (" " ++ explanation)
                    ]
                ]

        Nothing ->
            text ""


viewNavigationBar : ExerciseConfig msg -> ExerciseState -> Html msg
viewNavigationBar config state =
    let
        total =
            itemCount state

        prevDisabled =
            isFirstItem state

        onLast =
            isLastItem state

        allAnswered =
            allItemsAnswered state

        dots =
            List.map
                (\idx ->
                    let
                        isCurrent =
                            idx == state.currentIndex

                        correctness =
                            isItemCorrect idx state

                        dotColor =
                            if isCurrent then
                                "bg-indigo-400 ring-2 ring-indigo-400/50"

                            else
                                case correctness of
                                    Just True ->
                                        "bg-emerald-500"

                                    Just False ->
                                        "bg-rose-500"

                                    Nothing ->
                                        "bg-slate-600"
                    in
                    span [ Attr.class ("w-2.5 h-2.5 rounded-full " ++ dotColor) ] []
                )
                (List.range 0 (total - 1))
    in
    div [ Attr.class "flex flex-col items-center gap-2 w-full pt-2" ]
        [ div [ Attr.class "flex items-center justify-center gap-4" ]
            [ button
                [ Attr.class
                    (if prevDisabled then
                        "px-4 py-2 rounded-full bg-slate-800 text-slate-600 font-medium cursor-not-allowed"

                     else
                        "px-4 py-2 rounded-full bg-slate-700 text-slate-300 font-medium hover:bg-slate-600 transition-colors"
                    )
                , Attr.disabled prevDisabled
                , Events.onClick config.onPrevItem
                ]
                [ text "Prev" ]
            , div [ Attr.class "flex items-center gap-1.5" ] dots
            , if onLast then
                button
                    [ Attr.class
                        (if allAnswered then
                            "px-4 py-2 rounded-full bg-indigo-600 text-white font-medium hover:bg-indigo-500 transition-colors"

                         else
                            "px-4 py-2 rounded-full bg-slate-800 text-slate-600 font-medium cursor-not-allowed"
                        )
                    , Attr.disabled (not allAnswered)
                    , Events.onClick config.onFinishExercise
                    ]
                    [ text "Finish" ]

              else
                button
                    [ Attr.class "px-4 py-2 rounded-full bg-slate-700 text-slate-300 font-medium hover:bg-slate-600 transition-colors"
                    , Events.onClick config.onNextItem
                    ]
                    [ text "Next" ]
            ]
        , button
            [ Attr.class "text-xs text-slate-600 hover:text-rose-400 transition-colors"
            , Events.onClick config.onResetExercise
            ]
            [ text "Abandon exercise" ]
        ]


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

        translationBreakdown =
            case state.kind of
                Translation _ ->
                    let
                        ts =
                            translationScore state
                    in
                    [ p [ Attr.class "text-slate-400 text-sm" ]
                        [ text
                            (String.fromInt ts.perfect
                                ++ " perfect, "
                                ++ String.fromInt ts.good
                                ++ " good, "
                                ++ String.fromInt ts.wrong
                                ++ " wrong"
                            )
                        ]
                    ]

                TenseIdentification _ ->
                    []
    in
    div [ Attr.class "flex flex-col items-center gap-6 py-8" ]
        ([ p [ Attr.class "text-slate-400 text-sm uppercase tracking-wider" ]
            [ text "Exercise Complete" ]
         , div [ Attr.class ("text-5xl font-bold " ++ colorClass) ]
            [ text (String.fromInt score.correct ++ " / " ++ String.fromInt score.total) ]
         , p [ Attr.class "text-slate-400" ]
            [ text (String.fromInt (round pct) ++ "% correct") ]
         ]
            ++ translationBreakdown
            ++ [ button
                    [ Attr.class "px-6 py-3 rounded-full bg-indigo-600 text-white font-medium hover:bg-indigo-500 transition-colors"
                    , Events.onClick onReset
                    ]
                    [ text "Try Another" ]
               ]
        )
