module Exercise.Types exposing (..)

import Array exposing (Array)
import Grammar.Types exposing (VerbTense)


type ExerciseInputMode
    = WriteOwnMode
    | GenerateStoryMode
    | SavedStoriesMode


type ExercisePhase
    = Answering
    | ShowingFeedback
    | ExerciseComplete


type alias ExerciseItem =
    { original : String
    , correctTense : VerbTense
    , explanation : String
    }


type alias ExerciseState =
    { items : Array ExerciseItem
    , currentIndex : Int
    , answers : Array (Maybe VerbTense)
    , phase : ExercisePhase
    }


initExercise : List ExerciseItem -> ExerciseState
initExercise items =
    { items = Array.fromList items
    , currentIndex = 0
    , answers = Array.repeat (List.length items) Nothing
    , phase = Answering
    }


currentItem : ExerciseState -> Maybe ExerciseItem
currentItem state =
    Array.get state.currentIndex state.items


recordAnswer : VerbTense -> ExerciseState -> ExerciseState
recordAnswer vt state =
    { state
        | answers = Array.set state.currentIndex (Just vt) state.answers
        , phase = ShowingFeedback
    }


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

        correct =
            Array.toList (Array.indexedMap Tuple.pair state.items)
                |> List.filterMap
                    (\( idx, item ) ->
                        Array.get idx state.answers
                            |> Maybe.andThen identity
                            |> Maybe.map (\ans -> ans == item.correctTense)
                    )
                |> List.filter identity
                |> List.length
    in
    { correct = correct, total = total }
