module Api exposing (ApiError(..), EvaluationResult, LLMModel, analyzeNarrative, errorToString, evaluateTranslation, fetchModels, generateStory)

import Exercise.Types exposing (ExerciseItem)
import Grammar.Types exposing (VerbTense, verbTenseFromString, verbTenseLabel, verbTenseToString)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


type ApiError
    = Unauthorized
    | InsufficientCredits
    | RateLimited
    | NetworkError String
    | BadResponse String



-- =========================================================
-- System Prompts
-- =========================================================


analyzeSystemPrompt : String
analyzeSystemPrompt =
    """You are a language analysis assistant. The user will provide a short narrative written in a non-English language (such as Spanish, French, Japanese, etc.).

Your task:
1. Split the narrative into individual sentences.
2. For each sentence, determine the primary English verb tense that would be used to translate it.
3. Provide a brief explanation (1 sentence) of why that tense applies.

The verb tense MUST be one of these exact values:
SimplePast, PastContinuous, PastPerfect, PastPerfectContinuous,
SimplePresent, PresentContinuous, PresentPerfect, PresentPerfectContinuous,
SimpleFuture, FutureContinuous, FuturePerfect, FuturePerfectContinuous

Respond with a JSON object containing a "sentences" array."""


buildGenerateSystemPrompt : String -> List VerbTense -> String
buildGenerateSystemPrompt topic tenses =
    let
        tenseLabels =
            List.map verbTenseLabel tenses |> String.join ", "

        tenseEnums =
            List.map verbTenseToString tenses |> String.join ", "

        tenseCount =
            List.length tenses
    in
    "You are a language teaching assistant. The user will specify a non-English language. You MUST write a very short everyday story (5-7 sentences, no more than 10) entirely in that language — NOT in English. Every sentence in the \"original\" field must be written in the specified language. The story MUST be about: "
        ++ topic
        ++ ". Keep each sentence short and simple.\n\nIMPORTANT: You MUST use ONLY the following verb tenses (distribute them as evenly as possible across sentences): "
        ++ tenseLabels
        ++ ". Try to use at least "
        ++ String.fromInt (min tenseCount 4)
        ++ " different tenses.\n\nAfter writing the story, analyze each sentence and determine the primary English verb tense that would be used to translate it.\n\nThe verb tense MUST be one of these exact values:\n"
        ++ tenseEnums
        ++ "\n\nThe \"explanation\" field should be a brief English explanation (1 sentence) of why that tense applies.\n\nRespond with a JSON object containing a \"sentences\" array. The array MUST have at most 10 items."


evaluationSystemPrompt : String
evaluationSystemPrompt =
    """You are a language teaching assistant evaluating a student's English translation.

You will receive:
- The original sentence in a non-English language
- The expected English verb tense (e.g. "Simple Past", "Present Perfect")
- The student's English translation

Evaluate the translation as follows:
- "perfect": The translation is accurate, uses the correct verb tense, and reads naturally.
- "good": The translation captures the meaning and uses an acceptable verb tense, but has minor issues (e.g. slightly awkward phrasing, minor vocabulary choice).
- "wrong": The translation uses the wrong verb tense, significantly misses the meaning, or is incomprehensible.

Provide a brief explanation (1-2 sentences) of your rating.

Respond with a JSON object containing "rating" and "explanation"."""



-- =========================================================
-- Language Names
-- =========================================================


languageNameFromCode : String -> String
languageNameFromCode code =
    case code of
        "es" ->
            "Spanish"

        "fr" ->
            "French"

        "de" ->
            "German"

        "it" ->
            "Italian"

        "pt" ->
            "Portuguese"

        "ja" ->
            "Japanese"

        "ko" ->
            "Korean"

        "zh" ->
            "Chinese"

        "ru" ->
            "Russian"

        "ar" ->
            "Arabic"

        "hi" ->
            "Hindi"

        "tr" ->
            "Turkish"

        _ ->
            code



-- =========================================================
-- Response Schema (JSON Encode)
-- =========================================================


responseSchema : Encode.Value
responseSchema =
    Encode.object
        [ ( "type", Encode.string "json_schema" )
        , ( "json_schema"
          , Encode.object
                [ ( "name", Encode.string "tense_analysis" )
                , ( "strict", Encode.bool True )
                , ( "schema"
                  , Encode.object
                        [ ( "type", Encode.string "object" )
                        , ( "properties"
                          , Encode.object
                                [ ( "sentences"
                                  , Encode.object
                                        [ ( "type", Encode.string "array" )
                                        , ( "items"
                                          , Encode.object
                                                [ ( "type", Encode.string "object" )
                                                , ( "properties"
                                                  , Encode.object
                                                        [ ( "original", Encode.object [ ( "type", Encode.string "string" ) ] )
                                                        , ( "verbTense"
                                                          , Encode.object
                                                                [ ( "type", Encode.string "string" )
                                                                , ( "enum"
                                                                  , Encode.list Encode.string
                                                                        [ "SimplePast"
                                                                        , "PastContinuous"
                                                                        , "PastPerfect"
                                                                        , "PastPerfectContinuous"
                                                                        , "SimplePresent"
                                                                        , "PresentContinuous"
                                                                        , "PresentPerfect"
                                                                        , "PresentPerfectContinuous"
                                                                        , "SimpleFuture"
                                                                        , "FutureContinuous"
                                                                        , "FuturePerfect"
                                                                        , "FuturePerfectContinuous"
                                                                        ]
                                                                  )
                                                                ]
                                                          )
                                                        , ( "explanation", Encode.object [ ( "type", Encode.string "string" ) ] )
                                                        ]
                                                  )
                                                , ( "required", Encode.list Encode.string [ "original", "verbTense", "explanation" ] )
                                                , ( "additionalProperties", Encode.bool False )
                                                ]
                                          )
                                        ]
                                  )
                                ]
                          )
                        , ( "required", Encode.list Encode.string [ "sentences" ] )
                        , ( "additionalProperties", Encode.bool False )
                        ]
                  )
                ]
          )
        ]



-- =========================================================
-- Response Decoders
-- =========================================================


decodeLLMResponse : Decode.Decoder (List ExerciseItem)
decodeLLMResponse =
    Decode.field "sentences"
        (Decode.list decodeExerciseItem)


decodeExerciseItem : Decode.Decoder ExerciseItem
decodeExerciseItem =
    Decode.map3 ExerciseItem
        (Decode.field "original" Decode.string)
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


decodeOpenRouterContent : Decode.Decoder String
decodeOpenRouterContent =
    Decode.at [ "choices" ] (Decode.index 0 (Decode.at [ "message", "content" ] Decode.string))



-- =========================================================
-- HTTP Helpers
-- =========================================================


openRouterHeaders : String -> List Http.Header
openRouterHeaders apiKey =
    [ Http.header "Authorization" ("Bearer " ++ apiKey)
    , Http.header "HTTP-Referer" "https://nilp0inter.github.io/grammar-space/"
    , Http.header "X-OpenRouter-Title" "Grammar Space"
    ]


handleApiResponse : Http.Response String -> Result ApiError (List ExerciseItem)
handleApiResponse response =
    case response of
        Http.BadUrl_ url ->
            Err (NetworkError ("Bad URL: " ++ url))

        Http.Timeout_ ->
            Err (NetworkError "Request timed out")

        Http.NetworkError_ ->
            Err (NetworkError "Network error. Please check your connection.")

        Http.BadStatus_ metadata _ ->
            case metadata.statusCode of
                401 ->
                    Err Unauthorized

                402 ->
                    Err InsufficientCredits

                429 ->
                    Err RateLimited

                code ->
                    Err (BadResponse ("API error: " ++ String.fromInt code))

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decodeOpenRouterContent body of
                Err _ ->
                    Err (BadResponse "No content in API response.")

                Ok contentStr ->
                    case Decode.decodeString decodeLLMResponse contentStr of
                        Ok items ->
                            Ok items

                        Err decodeErr ->
                            Err (BadResponse ("Failed to parse response: " ++ Decode.errorToString decodeErr))


type alias EvaluationResult =
    { rating : String
    , explanation : String
    }


evaluationResponseSchema : Encode.Value
evaluationResponseSchema =
    Encode.object
        [ ( "type", Encode.string "json_schema" )
        , ( "json_schema"
          , Encode.object
                [ ( "name", Encode.string "translation_evaluation" )
                , ( "strict", Encode.bool True )
                , ( "schema"
                  , Encode.object
                        [ ( "type", Encode.string "object" )
                        , ( "properties"
                          , Encode.object
                                [ ( "rating"
                                  , Encode.object
                                        [ ( "type", Encode.string "string" )
                                        , ( "enum", Encode.list Encode.string [ "wrong", "good", "perfect" ] )
                                        ]
                                  )
                                , ( "explanation", Encode.object [ ( "type", Encode.string "string" ) ] )
                                ]
                          )
                        , ( "required", Encode.list Encode.string [ "rating", "explanation" ] )
                        , ( "additionalProperties", Encode.bool False )
                        ]
                  )
                ]
          )
        ]


decodeEvaluationResult : Decode.Decoder EvaluationResult
decodeEvaluationResult =
    Decode.map2 EvaluationResult
        (Decode.field "rating" Decode.string)
        (Decode.field "explanation" Decode.string)


handleEvaluationResponse : Http.Response String -> Result ApiError EvaluationResult
handleEvaluationResponse response =
    case response of
        Http.BadUrl_ url ->
            Err (NetworkError ("Bad URL: " ++ url))

        Http.Timeout_ ->
            Err (NetworkError "Request timed out")

        Http.NetworkError_ ->
            Err (NetworkError "Network error. Please check your connection.")

        Http.BadStatus_ metadata _ ->
            case metadata.statusCode of
                401 ->
                    Err Unauthorized

                402 ->
                    Err InsufficientCredits

                429 ->
                    Err RateLimited

                code ->
                    Err (BadResponse ("API error: " ++ String.fromInt code))

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decodeOpenRouterContent body of
                Err _ ->
                    Err (BadResponse "No content in API response.")

                Ok contentStr ->
                    case Decode.decodeString decodeEvaluationResult contentStr of
                        Ok result ->
                            Ok result

                        Err decodeErr ->
                            Err (BadResponse ("Failed to parse evaluation: " ++ Decode.errorToString decodeErr))



-- =========================================================
-- HTTP Functions
-- =========================================================


analyzeNarrative : { apiKey : String, narrative : String, model : String, onResult : Result ApiError (List ExerciseItem) -> msg } -> Cmd msg
analyzeNarrative { apiKey, narrative, model, onResult } =
    Http.request
        { method = "POST"
        , headers = openRouterHeaders apiKey
        , url = "https://openrouter.ai/api/v1/chat/completions"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "model", Encode.string model )
                    , ( "messages"
                      , Encode.list identity
                            [ Encode.object [ ( "role", Encode.string "system" ), ( "content", Encode.string analyzeSystemPrompt ) ]
                            , Encode.object [ ( "role", Encode.string "user" ), ( "content", Encode.string narrative ) ]
                            ]
                      )
                    , ( "response_format", responseSchema )
                    , ( "temperature", Encode.float 0.3 )
                    ]
                )
        , expect = Http.expectStringResponse onResult handleApiResponse
        , timeout = Nothing
        , tracker = Nothing
        }


generateStory : { apiKey : String, language : String, topic : String, tenses : List VerbTense, model : String, onResult : Result ApiError (List ExerciseItem) -> msg } -> Cmd msg
generateStory { apiKey, language, topic, tenses, model, onResult } =
    let
        languageName =
            languageNameFromCode language

        systemPrompt =
            buildGenerateSystemPrompt topic tenses
    in
    Http.request
        { method = "POST"
        , headers = openRouterHeaders apiKey
        , url = "https://openrouter.ai/api/v1/chat/completions"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "model", Encode.string model )
                    , ( "messages"
                      , Encode.list identity
                            [ Encode.object [ ( "role", Encode.string "system" ), ( "content", Encode.string systemPrompt ) ]
                            , Encode.object [ ( "role", Encode.string "user" ), ( "content", Encode.string ("Write the story in " ++ languageName ++ " about: " ++ topic ++ ".") ) ]
                            ]
                      )
                    , ( "response_format", responseSchema )
                    , ( "temperature", Encode.float 0.7 )
                    ]
                )
        , expect = Http.expectStringResponse onResult handleApiResponse
        , timeout = Nothing
        , tracker = Nothing
        }


evaluateTranslation :
    { apiKey : String
    , model : String
    , originalSentence : String
    , expectedTense : String
    , studentTranslation : String
    , onResult : Result ApiError EvaluationResult -> msg
    }
    -> Cmd msg
evaluateTranslation { apiKey, model, originalSentence, expectedTense, studentTranslation, onResult } =
    let
        userMessage =
            "Original sentence: " ++ originalSentence ++ "\nExpected English verb tense: " ++ expectedTense ++ "\nStudent's translation: " ++ studentTranslation
    in
    Http.request
        { method = "POST"
        , headers = openRouterHeaders apiKey
        , url = "https://openrouter.ai/api/v1/chat/completions"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "model", Encode.string model )
                    , ( "messages"
                      , Encode.list identity
                            [ Encode.object [ ( "role", Encode.string "system" ), ( "content", Encode.string evaluationSystemPrompt ) ]
                            , Encode.object [ ( "role", Encode.string "user" ), ( "content", Encode.string userMessage ) ]
                            ]
                      )
                    , ( "response_format", evaluationResponseSchema )
                    , ( "temperature", Encode.float 0.2 )
                    ]
                )
        , expect = Http.expectStringResponse onResult handleEvaluationResponse
        , timeout = Nothing
        , tracker = Nothing
        }


type alias LLMModel =
    { id : String, name : String }


handleModelsResponse : Http.Response String -> Result ApiError (List LLMModel)
handleModelsResponse response =
    case response of
        Http.BadUrl_ url ->
            Err (NetworkError ("Bad URL: " ++ url))

        Http.Timeout_ ->
            Err (NetworkError "Request timed out")

        Http.NetworkError_ ->
            Err (NetworkError "Network error")

        Http.BadStatus_ metadata _ ->
            case metadata.statusCode of
                401 ->
                    Err Unauthorized

                _ ->
                    Err (BadResponse ("API error: " ++ String.fromInt metadata.statusCode))

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decodeModelsResponse body of
                Ok models ->
                    Ok models

                Err _ ->
                    Ok []


decodeModelsResponse : Decode.Decoder (List LLMModel)
decodeModelsResponse =
    Decode.field "data"
        (Decode.list
            (Decode.map2 LLMModel
                (Decode.field "id" Decode.string)
                (Decode.oneOf
                    [ Decode.field "name" Decode.string
                    , Decode.field "id" Decode.string
                    ]
                )
            )
        )
        |> Decode.map (List.sortBy .name)


fetchModels : { apiKey : String, onResult : Result ApiError (List LLMModel) -> msg } -> Cmd msg
fetchModels { apiKey, onResult } =
    Http.request
        { method = "GET"
        , headers = openRouterHeaders apiKey
        , url = "https://openrouter.ai/api/v1/models"
        , body = Http.emptyBody
        , expect = Http.expectStringResponse onResult handleModelsResponse
        , timeout = Nothing
        , tracker = Nothing
        }



-- =========================================================
-- Error Message Helper
-- =========================================================


errorToString : ApiError -> String
errorToString err =
    case err of
        Unauthorized ->
            "API key is invalid or revoked. Please reconnect."

        InsufficientCredits ->
            "Insufficient credits on your OpenRouter account."

        RateLimited ->
            "Rate limited. Please wait a moment and try again."

        NetworkError msg ->
            msg

        BadResponse msg ->
            msg
