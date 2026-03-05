module Prompt exposing (viewPrompt)

import Html exposing (Html, span, text)
import Html.Attributes as Attr


type PromptSegment
    = PlainText String
    | VerbHighlight String


parsePrompt : String -> List PromptSegment
parsePrompt input =
    parseHelp input [] ""


parseHelp : String -> List PromptSegment -> String -> List PromptSegment
parseHelp remaining segments currentPlain =
    case String.uncons remaining of
        Nothing ->
            if String.isEmpty currentPlain then
                List.reverse segments

            else
                List.reverse (PlainText currentPlain :: segments)

        Just ( '[', rest ) ->
            case String.uncons rest of
                Just ( '[', afterOpen ) ->
                    let
                        ( verb, afterClose ) =
                            extractUntilClose afterOpen
                    in
                    if String.isEmpty currentPlain then
                        parseHelp afterClose (VerbHighlight verb :: segments) ""

                    else
                        parseHelp afterClose (VerbHighlight verb :: PlainText currentPlain :: segments) ""

                _ ->
                    parseHelp rest segments (currentPlain ++ "[")

        Just ( c, rest ) ->
            parseHelp rest segments (currentPlain ++ String.fromChar c)


extractUntilClose : String -> ( String, String )
extractUntilClose input =
    extractUntilCloseHelp input ""


extractUntilCloseHelp : String -> String -> ( String, String )
extractUntilCloseHelp remaining acc =
    case String.uncons remaining of
        Nothing ->
            ( acc, "" )

        Just ( ']', rest ) ->
            case String.uncons rest of
                Just ( ']', afterClose ) ->
                    ( acc, afterClose )

                _ ->
                    extractUntilCloseHelp rest (acc ++ "]")

        Just ( c, rest ) ->
            extractUntilCloseHelp rest (acc ++ String.fromChar c)


viewPrompt : String -> List (Html msg)
viewPrompt input =
    List.map viewSegment (parsePrompt input)


viewSegment : PromptSegment -> Html msg
viewSegment segment =
    case segment of
        PlainText s ->
            text s

        VerbHighlight verb ->
            span
                [ Attr.class "bg-amber-500/20 text-amber-300 font-semibold rounded-md px-2 py-0.5" ]
                [ text verb ]
