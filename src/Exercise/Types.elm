module Exercise.Types exposing (..)

import Array exposing (Array)
import Grammar.Types exposing (VerbTense)


type ExerciseInputMode
    = WriteOwnMode
    | GenerateStoryMode
    | SavedStoriesMode


type ExerciseTypeChoice
    = TenseIdChoice
    | TranslationChoice


type ExercisePhase
    = Answering
    | ShowingFeedback
    | ExerciseComplete


type alias ExerciseItem =
    { original : String
    , correctTense : VerbTense
    , explanation : String
    }


type ExerciseKind
    = TenseIdentification { answers : Array (Maybe VerbTense) }
    | Translation TranslationState


type alias TranslationState =
    { inputs : Array String
    , evaluations : Array (Maybe TranslationEvaluation)
    , evaluating : Bool
    }


type TranslationEvaluation
    = Wrong String
    | Good String
    | Perfect String


type alias ExerciseState =
    { items : Array ExerciseItem
    , currentIndex : Int
    , phase : ExercisePhase
    , kind : ExerciseKind
    }


initTenseIdExercise : List ExerciseItem -> ExerciseState
initTenseIdExercise items =
    { items = Array.fromList items
    , currentIndex = 0
    , phase = Answering
    , kind = TenseIdentification { answers = Array.repeat (List.length items) Nothing }
    }


initTranslationExercise : List ExerciseItem -> ExerciseState
initTranslationExercise items =
    { items = Array.fromList items
    , currentIndex = 0
    , phase = Answering
    , kind =
        Translation
            { inputs = Array.repeat (List.length items) ""
            , evaluations = Array.repeat (List.length items) Nothing
            , evaluating = False
            }
    }


currentItem : ExerciseState -> Maybe ExerciseItem
currentItem state =
    Array.get state.currentIndex state.items


recordTenseAnswer : VerbTense -> ExerciseState -> ExerciseState
recordTenseAnswer vt state =
    case state.kind of
        TenseIdentification ti ->
            { state
                | kind = TenseIdentification { ti | answers = Array.set state.currentIndex (Just vt) ti.answers }
                , phase = ShowingFeedback
            }

        Translation _ ->
            state


updateTranslationInput : String -> ExerciseState -> ExerciseState
updateTranslationInput input state =
    case state.kind of
        Translation ts ->
            { state
                | kind = Translation { ts | inputs = Array.set state.currentIndex input ts.inputs }
            }

        TenseIdentification _ ->
            state


setTranslationEvaluating : Bool -> ExerciseState -> ExerciseState
setTranslationEvaluating evaluating state =
    case state.kind of
        Translation ts ->
            { state | kind = Translation { ts | evaluating = evaluating } }

        TenseIdentification _ ->
            state


recordTranslationEvaluation : TranslationEvaluation -> ExerciseState -> ExerciseState
recordTranslationEvaluation eval state =
    case state.kind of
        Translation ts ->
            { state
                | kind =
                    Translation
                        { ts
                            | evaluations = Array.set state.currentIndex (Just eval) ts.evaluations
                            , evaluating = False
                        }
                , phase = ShowingFeedback
            }

        TenseIdentification _ ->
            state


advanceToNext : ExerciseState -> ExerciseState
advanceToNext state =
    let
        nextIdx =
            state.currentIndex + 1
    in
    if nextIdx >= Array.length state.items then
        { state | phase = ExerciseComplete }

    else
        { state | currentIndex = nextIdx, phase = Answering }


scoreExercise : ExerciseState -> { correct : Int, total : Int }
scoreExercise state =
    let
        total =
            Array.length state.items
    in
    case state.kind of
        TenseIdentification ti ->
            let
                correct =
                    Array.toList (Array.indexedMap Tuple.pair state.items)
                        |> List.filterMap
                            (\( idx, item ) ->
                                Array.get idx ti.answers
                                    |> Maybe.andThen identity
                                    |> Maybe.map (\ans -> ans == item.correctTense)
                            )
                        |> List.filter identity
                        |> List.length
            in
            { correct = correct, total = total }

        Translation ts ->
            let
                correct =
                    Array.toList ts.evaluations
                        |> List.filterMap identity
                        |> List.filter
                            (\eval ->
                                case eval of
                                    Good _ ->
                                        True

                                    Perfect _ ->
                                        True

                                    Wrong _ ->
                                        False
                            )
                        |> List.length
            in
            { correct = correct, total = total }


translationScore : ExerciseState -> { perfect : Int, good : Int, wrong : Int, total : Int }
translationScore state =
    case state.kind of
        Translation ts ->
            let
                evals =
                    Array.toList ts.evaluations |> List.filterMap identity

                countBy f =
                    List.filter f evals |> List.length
            in
            { perfect =
                countBy
                    (\e ->
                        case e of
                            Perfect _ ->
                                True

                            _ ->
                                False
                    )
            , good =
                countBy
                    (\e ->
                        case e of
                            Good _ ->
                                True

                            _ ->
                                False
                    )
            , wrong =
                countBy
                    (\e ->
                        case e of
                            Wrong _ ->
                                True

                            _ ->
                                False
                    )
            , total = Array.length state.items
            }

        TenseIdentification _ ->
            { perfect = 0, good = 0, wrong = 0, total = Array.length state.items }


type StoryTopic
    = PredefinedTopic String
    | CustomTopic


storyTopics : List { id : String, label : String }
storyTopics =
    [ { id = "daily-routine", label = "Daily Routine" }
    , { id = "grocery-shopping", label = "Grocery Shopping" }
    , { id = "cooking-dinner", label = "Cooking Dinner" }
    , { id = "commute-to-work", label = "Commute to Work" }
    , { id = "weekend-plans", label = "Weekend Plans" }
    , { id = "doctor-visit", label = "Doctor Visit" }
    , { id = "meeting-a-friend", label = "Meeting a Friend" }
    , { id = "moving-to-a-new-city", label = "Moving to a New City" }
    , { id = "job-interview", label = "Job Interview" }
    , { id = "family-dinner", label = "Family Dinner" }
    , { id = "planning-a-vacation", label = "Planning a Vacation" }
    , { id = "a-rainy-day", label = "A Rainy Day" }
    , { id = "birthday-party", label = "Birthday Party" }
    , { id = "learning-a-new-skill", label = "Learning a New Skill" }
    , { id = "getting-lost-in-a-city", label = "Getting Lost in a City" }
    , { id = "taking-care-of-a-pet", label = "Taking Care of a Pet" }
    , { id = "a-day-at-school", label = "A Day at School" }
    , { id = "cleaning-the-house", label = "Cleaning the House" }
    , { id = "eating-at-a-restaurant", label = "Eating at a Restaurant" }
    , { id = "morning-exercise", label = "Morning Exercise" }
    ]
