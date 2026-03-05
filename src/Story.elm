module Story exposing (SavedStory, StoryMode(..), buildSavedStory, decodeSavedStories, encodeSavedStories)

import Exercise.Types exposing (ExerciseItem)
import Grammar.Types exposing (VerbTense, verbTenseFromString, verbTenseToString)
import Json.Decode as Decode
import Json.Encode as Encode


type alias SavedStory =
    { id : String
    , title : String
    , language : String
    , mode : StoryMode
    , items : List ExerciseItem
    , createdAt : Int
    }


type StoryMode
    = UserWritten String
    | AIGenerated



-- =========================================================
-- Build
-- =========================================================


buildSavedStory : { language : String, mode : StoryMode, items : List ExerciseItem, nowMillis : Int } -> SavedStory
buildSavedStory { language, mode, items, nowMillis } =
    let
        title =
            List.head items
                |> Maybe.map .original
                |> Maybe.withDefault "Untitled"
                |> String.left 60
    in
    { id = String.fromInt nowMillis
    , title = title
    , language = language
    , mode = mode
    , items = items
    , createdAt = nowMillis
    }



-- =========================================================
-- JSON Encode
-- =========================================================


encodeSavedStories : List SavedStory -> Encode.Value
encodeSavedStories stories =
    Encode.list encodeSavedStory stories


encodeSavedStory : SavedStory -> Encode.Value
encodeSavedStory story =
    Encode.object
        [ ( "id", Encode.string story.id )
        , ( "title", Encode.string story.title )
        , ( "language", Encode.string story.language )
        , ( "mode", encodeStoryMode story.mode )
        , ( "items", Encode.list encodeExerciseItem story.items )
        , ( "createdAt", Encode.int story.createdAt )
        ]


encodeStoryMode : StoryMode -> Encode.Value
encodeStoryMode mode =
    case mode of
        UserWritten narrative ->
            Encode.object
                [ ( "type", Encode.string "userWritten" )
                , ( "narrative", Encode.string narrative )
                ]

        AIGenerated ->
            Encode.object
                [ ( "type", Encode.string "aiGenerated" )
                ]


encodeExerciseItem : ExerciseItem -> Encode.Value
encodeExerciseItem item =
    Encode.object
        [ ( "original", Encode.string item.original )
        , ( "prompt", Encode.string item.prompt )
        , ( "verbTense", Encode.string (verbTenseToString item.correctTense) )
        , ( "explanation", Encode.string item.explanation )
        ]



-- =========================================================
-- JSON Decode
-- =========================================================


decodeSavedStories : Decode.Decoder (List SavedStory)
decodeSavedStories =
    Decode.list decodeSavedStory


decodeSavedStory : Decode.Decoder SavedStory
decodeSavedStory =
    Decode.map6 SavedStory
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "language" Decode.string)
        (Decode.field "mode" decodeStoryMode)
        (Decode.field "items" (Decode.list decodeExerciseItem))
        (Decode.field "createdAt" Decode.int)


decodeStoryMode : Decode.Decoder StoryMode
decodeStoryMode =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "userWritten" ->
                        Decode.map UserWritten (Decode.field "narrative" Decode.string)

                    "aiGenerated" ->
                        Decode.succeed AIGenerated

                    _ ->
                        Decode.fail ("Unknown story mode: " ++ t)
            )


decodeExerciseItem : Decode.Decoder ExerciseItem
decodeExerciseItem =
    Decode.map4 ExerciseItem
        (Decode.field "original" Decode.string)
        (Decode.field "prompt" Decode.string)
        (Decode.field "verbTense" decodeVerbTense)
        (Decode.field "explanation" Decode.string)


decodeVerbTense : Decode.Decoder VerbTense
decodeVerbTense =
    Decode.string
        |> Decode.andThen
            (\s ->
                case verbTenseFromString s of
                    Just vt ->
                        Decode.succeed vt

                    Nothing ->
                        Decode.fail ("Unknown verb tense: " ++ s)
            )
