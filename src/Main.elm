module Main exposing (main)

import Animator
import Animator.Inline
import Browser
import Grammar.Engine exposing (nullArgs)
import Grammar.Lexicon as Lexicon
import Grammar.Render as Render
import Grammar.Types exposing (..)
import Html exposing (Html, div, h1, p, span, text)
import Html.Attributes as Attr
import Html.Events as Events
import Time



-- =========================================================
-- SentencePhase for animation
-- =========================================================


type SentencePhase
    = Visible
    | Refreshing



-- =========================================================
-- Model
-- =========================================================


type alias Model =
    { clauseMode : ClauseMode
    , subject : SubjectNP
    , verb : Verb
    , arguments : Arguments
    , finiteSpec : FiniteSpec
    , imperativeSpec : ImperativeSpec
    , infinitiveSpec : { perfect : Bool, progressive : Bool, voice : Voice, polarity : Polarity }
    , gerundSpec : { perfect : Bool, voice : Voice, polarity : Polarity }
    , participleForm : ParticipleForm
    , participlePol : Polarity
    , style : Style
    , selectedSubjectIdx : Int
    , selectedVerbIdx : Int
    , sentenceTimeline : Animator.Timeline SentencePhase
    , result : Result String (List SentenceWord)
    }



-- =========================================================
-- Msg
-- =========================================================


type Msg
    = Tick Time.Posix
    | SetClauseMode ClauseMode
    | SetTense Tense
    | SetModal (Maybe Modality)
    | TogglePerfect
    | ToggleProgressive
    | SetVoice Voice
    | SetPolarity Polarity
    | SetStyle Style
    | SetClauseType ClauseType
    | SetSubject Int
    | SetVerb Int
    | SetArgObj String
    | SetArgIndObj String
    | SetArgBy String
    | SetArgPred String
    | SetArgClause String
    | SetParticipleForm ParticipleForm



-- =========================================================
-- Animator
-- =========================================================


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watchingWith
            .sentenceTimeline
            (\newTl model -> { model | sentenceTimeline = newTl })
            (always False)



-- =========================================================
-- Init
-- =========================================================


init : () -> ( Model, Cmd Msg )
init _ =
    let
        defaultVerb =
            List.head Lexicon.verbs
                |> Maybe.withDefault
                    (Verb "be" "is" "was" "been" "being" Copular Predicative True)

        defaultSubj =
            List.head Lexicon.subjects
                |> Maybe.map Tuple.second
                |> Maybe.withDefault (RefNP (NP "I" First Singular))

        model =
            { clauseMode = FiniteMode
            , subject = defaultSubj
            , verb = defaultVerb
            , arguments = Lexicon.defaultArguments defaultVerb
            , finiteSpec =
                { fsTense = Present
                , fsModal = Nothing
                , fsPerfect = False
                , fsProgressive = False
                , fsVoice = Active
                , fsPolarity = Affirmative
                , fsClauseType = Declarative
                }
            , imperativeSpec =
                { isPerfect = False
                , isProgressive = False
                , isVoice = Active
                , isPolarity = Affirmative
                }
            , infinitiveSpec =
                { perfect = False
                , progressive = False
                , voice = Active
                , polarity = Affirmative
                }
            , gerundSpec =
                { perfect = False
                , voice = Active
                , polarity = Affirmative
                }
            , participleForm = PresParticipleActive
            , participlePol = Affirmative
            , style = Full
            , selectedSubjectIdx = 0
            , selectedVerbIdx = 0
            , sentenceTimeline = Animator.init Visible
            , result = Ok []
            }
    in
    ( recompute model, Cmd.none )



-- =========================================================
-- Recompute
-- =========================================================


recompute : Model -> Model
recompute model =
    let
        result =
            case model.clauseMode of
                FiniteMode ->
                    Render.renderFiniteWords model.style model.subject model.verb model.arguments model.finiteSpec

                ImperativeMode ->
                    Render.renderImperativeWords model.style model.verb model.arguments model.imperativeSpec

                InfinitiveMode ->
                    Render.renderNonFiniteWords model.verb
                        model.arguments
                        (Infinitive model.infinitiveSpec)

                GerundMode ->
                    Render.renderNonFiniteWords model.verb
                        model.arguments
                        (Gerund model.gerundSpec)

                ParticipleMode ->
                    Render.renderNonFiniteWords model.verb
                        model.arguments
                        (Participle { form = model.participleForm, polarity = model.participlePol })

        newTimeline =
            model.sentenceTimeline
                |> Animator.interrupt
                    [ Animator.event Animator.veryQuickly Refreshing
                    , Animator.event Animator.veryQuickly Visible
                    ]
    in
    { model | result = result, sentenceTimeline = newTimeline }



-- =========================================================
-- Update
-- =========================================================


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( model |> Animator.update newTime animator, Cmd.none )

        SetClauseMode mode ->
            ( recompute { model | clauseMode = mode }, Cmd.none )

        SetTense t ->
            let
                fs =
                    model.finiteSpec
            in
            ( recompute { model | finiteSpec = { fs | fsTense = t } }, Cmd.none )

        SetModal m ->
            let
                fs =
                    model.finiteSpec
            in
            ( recompute { model | finiteSpec = { fs | fsModal = m } }, Cmd.none )

        TogglePerfect ->
            ( recompute (togglePerfect model), Cmd.none )

        ToggleProgressive ->
            ( recompute (toggleProgressive model), Cmd.none )

        SetVoice v ->
            ( recompute (setVoice v model), Cmd.none )

        SetPolarity p ->
            ( recompute (setPolarity p model), Cmd.none )

        SetStyle s ->
            ( recompute { model | style = s }, Cmd.none )

        SetClauseType ct ->
            let
                fs =
                    model.finiteSpec
            in
            ( recompute { model | finiteSpec = { fs | fsClauseType = ct } }, Cmd.none )

        SetSubject idx ->
            let
                subj =
                    Lexicon.subjects
                        |> List.drop idx
                        |> List.head
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault model.subject
            in
            ( recompute { model | subject = subj, selectedSubjectIdx = idx }, Cmd.none )

        SetVerb idx ->
            let
                v =
                    Lexicon.verbs
                        |> List.drop idx
                        |> List.head
                        |> Maybe.withDefault model.verb

                newArgs =
                    Lexicon.defaultArguments v

                argsByMerge =
                    case v.vTrans of
                        _ ->
                            { newArgs | argBy = model.arguments.argBy }
            in
            ( recompute { model | verb = v, arguments = argsByMerge, selectedVerbIdx = idx }, Cmd.none )

        SetArgObj val ->
            let
                args =
                    model.arguments

                newObj =
                    if String.isEmpty val then
                        Nothing

                    else
                        Just (NP val Third Singular)
            in
            ( recompute { model | arguments = { args | argObj = newObj } }, Cmd.none )

        SetArgIndObj val ->
            let
                args =
                    model.arguments

                newInd =
                    if String.isEmpty val then
                        Nothing

                    else
                        Just (NP val Third Singular)
            in
            ( recompute { model | arguments = { args | argIndObj = newInd } }, Cmd.none )

        SetArgBy val ->
            let
                args =
                    model.arguments

                newBy =
                    if String.isEmpty val then
                        Nothing

                    else
                        Just (NP val Third Singular)
            in
            ( recompute { model | arguments = { args | argBy = newBy } }, Cmd.none )

        SetArgPred val ->
            let
                args =
                    model.arguments

                newPred =
                    if String.isEmpty val then
                        Nothing

                    else
                        Just val
            in
            ( recompute { model | arguments = { args | argPred = newPred } }, Cmd.none )

        SetArgClause val ->
            let
                args =
                    model.arguments

                newClause =
                    if String.isEmpty val then
                        Nothing

                    else
                        Just val
            in
            ( recompute { model | arguments = { args | argClause = newClause } }, Cmd.none )

        SetParticipleForm pf ->
            ( recompute { model | participleForm = pf }, Cmd.none )


togglePerfect : Model -> Model
togglePerfect model =
    case model.clauseMode of
        FiniteMode ->
            let
                fs =
                    model.finiteSpec
            in
            { model | finiteSpec = { fs | fsPerfect = not fs.fsPerfect } }

        ImperativeMode ->
            let
                is =
                    model.imperativeSpec
            in
            { model | imperativeSpec = { is | isPerfect = not is.isPerfect } }

        InfinitiveMode ->
            let
                s =
                    model.infinitiveSpec
            in
            { model | infinitiveSpec = { s | perfect = not s.perfect } }

        GerundMode ->
            let
                s =
                    model.gerundSpec
            in
            { model | gerundSpec = { s | perfect = not s.perfect } }

        ParticipleMode ->
            model


toggleProgressive : Model -> Model
toggleProgressive model =
    case model.clauseMode of
        FiniteMode ->
            let
                fs =
                    model.finiteSpec
            in
            { model | finiteSpec = { fs | fsProgressive = not fs.fsProgressive } }

        ImperativeMode ->
            let
                is =
                    model.imperativeSpec
            in
            { model | imperativeSpec = { is | isProgressive = not is.isProgressive } }

        InfinitiveMode ->
            let
                s =
                    model.infinitiveSpec
            in
            { model | infinitiveSpec = { s | progressive = not s.progressive } }

        _ ->
            model


setVoice : Voice -> Model -> Model
setVoice v model =
    case model.clauseMode of
        FiniteMode ->
            let
                fs =
                    model.finiteSpec
            in
            { model | finiteSpec = { fs | fsVoice = v } }

        ImperativeMode ->
            let
                is =
                    model.imperativeSpec
            in
            { model | imperativeSpec = { is | isVoice = v } }

        InfinitiveMode ->
            let
                s =
                    model.infinitiveSpec
            in
            { model | infinitiveSpec = { s | voice = v } }

        GerundMode ->
            let
                s =
                    model.gerundSpec
            in
            { model | gerundSpec = { s | voice = v } }

        ParticipleMode ->
            model


setPolarity : Polarity -> Model -> Model
setPolarity p model =
    case model.clauseMode of
        FiniteMode ->
            let
                fs =
                    model.finiteSpec
            in
            { model | finiteSpec = { fs | fsPolarity = p } }

        ImperativeMode ->
            let
                is =
                    model.imperativeSpec
            in
            { model | imperativeSpec = { is | isPolarity = p } }

        InfinitiveMode ->
            let
                s =
                    model.infinitiveSpec
            in
            { model | infinitiveSpec = { s | polarity = p } }

        GerundMode ->
            let
                s =
                    model.gerundSpec
            in
            { model | gerundSpec = { s | polarity = p } }

        ParticipleMode ->
            { model | participlePol = p }



-- =========================================================
-- Subscriptions
-- =========================================================


subscriptions : Model -> Sub Msg
subscriptions model =
    Animator.toSubscription Tick model animator



-- =========================================================
-- View
-- =========================================================


view : Model -> Html Msg
view model =
    div [ Attr.class "flex flex-col items-center min-h-screen py-10 px-4 gap-8 max-w-4xl mx-auto" ]
        [ viewTitle
        , viewSentence model
        , viewError model
        , viewLegend
        , viewControlPanel model
        ]


viewTitle : Html Msg
viewTitle =
    h1 [ Attr.class "text-4xl font-bold tracking-tight text-white" ]
        [ text "Grammar Space" ]


viewSentence : Model -> Html Msg
viewSentence model =
    div
        [ Attr.class "flex flex-wrap items-center justify-center gap-2 min-h-[4rem] p-6 rounded-xl bg-slate-800/50 w-full"
        , Animator.Inline.opacity model.sentenceTimeline <|
            \phase ->
                case phase of
                    Visible ->
                        Animator.at 1

                    Refreshing ->
                        Animator.at 0.3
        , Animator.Inline.scale model.sentenceTimeline <|
            \phase ->
                case phase of
                    Visible ->
                        Animator.at 1

                    Refreshing ->
                        Animator.at 0.97
        ]
        (case model.result of
            Ok words ->
                List.map viewWordPill words

            Err _ ->
                [ span [ Attr.class "text-slate-500 italic" ] [ text "..." ] ]
        )


viewWordPill : SentenceWord -> Html Msg
viewWordPill word =
    let
        ( bgClass, textClass ) =
            roleColors word.role
    in
    if word.role == PunctuationWord then
        span [ Attr.class "text-slate-400 text-xl font-medium -ml-1" ]
            [ text word.text ]

    else
        span
            [ Attr.class ("inline-flex items-center px-3 py-1.5 rounded-full text-sm font-medium " ++ bgClass ++ " " ++ textClass)
            ]
            [ text word.text ]


roleColors : WordRole -> ( String, String )
roleColors role =
    case role of
        SubjectWord ->
            ( "bg-blue-500/20", "text-blue-300" )

        Aux1Word ->
            ( "bg-amber-500/20", "text-amber-300" )

        NegWord ->
            ( "bg-rose-500/20", "text-rose-300" )

        AuxRWord ->
            ( "bg-yellow-500/20", "text-yellow-300" )

        MainVerbWord ->
            ( "bg-emerald-500/20", "text-emerald-300" )

        ComplementWord ->
            ( "bg-violet-500/20", "text-violet-300" )

        WhWord ->
            ( "bg-cyan-500/20", "text-cyan-300" )

        TagWord ->
            ( "bg-pink-500/20", "text-pink-300" )

        ComplementizerWord ->
            ( "bg-cyan-500/20", "text-cyan-300" )

        RelWord ->
            ( "bg-cyan-500/20", "text-cyan-300" )

        PunctuationWord ->
            ( "", "text-slate-400" )


viewError : Model -> Html Msg
viewError model =
    case model.result of
        Err msg ->
            div [ Attr.class "w-full px-4 py-3 rounded-lg bg-rose-500/10 border border-rose-500/30 text-rose-300 text-sm" ]
                [ text msg ]

        Ok _ ->
            text ""


viewLegend : Html Msg
viewLegend =
    div [ Attr.class "flex flex-wrap gap-3 justify-center text-xs" ]
        (List.map viewLegendItem
            [ ( "Subject", "bg-blue-500/20 text-blue-300" )
            , ( "Aux", "bg-amber-500/20 text-amber-300" )
            , ( "Neg", "bg-rose-500/20 text-rose-300" )
            , ( "Aux (rest)", "bg-yellow-500/20 text-yellow-300" )
            , ( "Main Verb", "bg-emerald-500/20 text-emerald-300" )
            , ( "Complement", "bg-violet-500/20 text-violet-300" )
            , ( "Wh-word", "bg-cyan-500/20 text-cyan-300" )
            , ( "Tag", "bg-pink-500/20 text-pink-300" )
            ]
        )


viewLegendItem : ( String, String ) -> Html Msg
viewLegendItem ( label, colorClass ) =
    span [ Attr.class ("inline-flex items-center gap-1.5 px-2 py-1 rounded " ++ colorClass) ]
        [ text label ]



-- =========================================================
-- Control Panel
-- =========================================================


viewControlPanel : Model -> Html Msg
viewControlPanel model =
    div [ Attr.class "flex flex-col gap-4 w-full" ]
        ([ viewRow "Clause Mode" (viewClauseModeRow model)
         ]
            ++ (case model.clauseMode of
                    FiniteMode ->
                        [ viewRow "Tense" (viewTenseRow model)
                        , viewRow "Modal" (viewModalRow model)
                        , viewRow "Aspect" (viewAspectRow model)
                        , viewRow "Voice" (viewVoiceRow model)
                        , viewRow "Polarity" (viewPolarityRow model)
                        , viewRow "Style" (viewStyleRow model)
                        , viewRow "Clause Type" (viewClauseTypeRow model)
                        , viewRow "Subject" (viewSubjectRow model)
                        , viewRow "Verb" (viewVerbRow model)
                        , viewRow "Arguments" (viewArgsRow model)
                        ]

                    ImperativeMode ->
                        [ viewRow "Aspect" (viewAspectRow model)
                        , viewRow "Voice" (viewVoiceRow model)
                        , viewRow "Polarity" (viewPolarityRow model)
                        , viewRow "Style" (viewStyleRow model)
                        , viewRow "Verb" (viewVerbRow model)
                        , viewRow "Arguments" (viewArgsRow model)
                        ]

                    InfinitiveMode ->
                        [ viewRow "Aspect" (viewAspectRow model)
                        , viewRow "Voice" (viewVoiceRow model)
                        , viewRow "Polarity" (viewPolarityRow model)
                        , viewRow "Verb" (viewVerbRow model)
                        , viewRow "Arguments" (viewArgsRow model)
                        ]

                    GerundMode ->
                        [ viewRow "Aspect" (viewGerundAspectRow model)
                        , viewRow "Voice" (viewVoiceRow model)
                        , viewRow "Polarity" (viewPolarityRow model)
                        , viewRow "Verb" (viewVerbRow model)
                        , viewRow "Arguments" (viewArgsRow model)
                        ]

                    ParticipleMode ->
                        [ viewRow "Form" (viewParticipleFormRow model)
                        , viewRow "Polarity" (viewPolarityRow model)
                        , viewRow "Verb" (viewVerbRow model)
                        , viewRow "Arguments" (viewArgsRow model)
                        ]
               )
        )


viewRow : String -> Html Msg -> Html Msg
viewRow label content =
    div [ Attr.class "flex flex-col gap-2" ]
        [ span [ Attr.class "text-xs font-medium text-slate-500 uppercase tracking-wider" ]
            [ text label ]
        , content
        ]


viewClauseModeRow : Model -> Html Msg
viewClauseModeRow model =
    div [ Attr.class "flex flex-wrap gap-2" ]
        [ pill (model.clauseMode == FiniteMode) False (SetClauseMode FiniteMode) "Finite"
        , pill (model.clauseMode == ImperativeMode) False (SetClauseMode ImperativeMode) "Imperative"
        , pill (model.clauseMode == InfinitiveMode) False (SetClauseMode InfinitiveMode) "Infinitive"
        , pill (model.clauseMode == GerundMode) False (SetClauseMode GerundMode) "Gerund"
        , pill (model.clauseMode == ParticipleMode) False (SetClauseMode ParticipleMode) "Participle"
        ]


viewTenseRow : Model -> Html Msg
viewTenseRow model =
    let
        fs =
            model.finiteSpec
    in
    div [ Attr.class "flex flex-wrap gap-2" ]
        [ pill (fs.fsTense == Past) False (SetTense Past) "Past"
        , pill (fs.fsTense == Present) False (SetTense Present) "Present"
        , pill (fs.fsTense == Future) False (SetTense Future) "Future"
        ]


viewModalRow : Model -> Html Msg
viewModalRow model =
    let
        fs =
            model.finiteSpec
    in
    div [ Attr.class "flex flex-wrap gap-2" ]
        [ pill (fs.fsModal == Nothing) False (SetModal Nothing) "None"
        , pill (fs.fsModal == Just Can) False (SetModal (Just Can)) "can"
        , pill (fs.fsModal == Just Should) False (SetModal (Just Should)) "should"
        , pill (fs.fsModal == Just Must) False (SetModal (Just Must)) "must"
        , pill (fs.fsModal == Just Would) False (SetModal (Just Would)) "would"
        , pill (fs.fsModal == Just May) False (SetModal (Just May)) "may"
        , pill (fs.fsModal == Just Might) False (SetModal (Just Might)) "might"
        , pill (fs.fsModal == Just Will) False (SetModal (Just Will)) "will"
        ]


viewAspectRow : Model -> Html Msg
viewAspectRow model =
    let
        ( perf, prog ) =
            currentAspect model

        isStative =
            not model.verb.vAllowsProg
    in
    div [ Attr.class "flex flex-wrap gap-2" ]
        [ togglePill perf False TogglePerfect "Perfect"
        , togglePill prog isStative ToggleProgressive "Progressive"
        ]


viewGerundAspectRow : Model -> Html Msg
viewGerundAspectRow model =
    div [ Attr.class "flex flex-wrap gap-2" ]
        [ togglePill model.gerundSpec.perfect False TogglePerfect "Perfect"
        ]


currentAspect : Model -> ( Bool, Bool )
currentAspect model =
    case model.clauseMode of
        FiniteMode ->
            ( model.finiteSpec.fsPerfect, model.finiteSpec.fsProgressive )

        ImperativeMode ->
            ( model.imperativeSpec.isPerfect, model.imperativeSpec.isProgressive )

        InfinitiveMode ->
            ( model.infinitiveSpec.perfect, model.infinitiveSpec.progressive )

        GerundMode ->
            ( model.gerundSpec.perfect, False )

        ParticipleMode ->
            ( False, False )


viewVoiceRow : Model -> Html Msg
viewVoiceRow model =
    let
        v =
            currentVoice model
    in
    div [ Attr.class "flex flex-wrap gap-2" ]
        [ pill (v == Active) False (SetVoice Active) "Active"
        , pill (v == Passive) False (SetVoice Passive) "Passive"
        ]


currentVoice : Model -> Voice
currentVoice model =
    case model.clauseMode of
        FiniteMode ->
            model.finiteSpec.fsVoice

        ImperativeMode ->
            model.imperativeSpec.isVoice

        InfinitiveMode ->
            model.infinitiveSpec.voice

        GerundMode ->
            model.gerundSpec.voice

        ParticipleMode ->
            Active


viewPolarityRow : Model -> Html Msg
viewPolarityRow model =
    let
        p =
            currentPolarity model
    in
    div [ Attr.class "flex flex-wrap gap-2" ]
        [ pill (p == Affirmative) False (SetPolarity Affirmative) "Affirmative"
        , pill (p == Negative) False (SetPolarity Negative) "Negative"
        ]


currentPolarity : Model -> Polarity
currentPolarity model =
    case model.clauseMode of
        FiniteMode ->
            model.finiteSpec.fsPolarity

        ImperativeMode ->
            model.imperativeSpec.isPolarity

        InfinitiveMode ->
            model.infinitiveSpec.polarity

        GerundMode ->
            model.gerundSpec.polarity

        ParticipleMode ->
            model.participlePol


viewStyleRow : Model -> Html Msg
viewStyleRow model =
    div [ Attr.class "flex flex-wrap gap-2" ]
        [ pill (model.style == Full) False (SetStyle Full) "Full"
        , pill (model.style == Contracted) False (SetStyle Contracted) "Contracted"
        ]


viewClauseTypeRow : Model -> Html Msg
viewClauseTypeRow model =
    let
        ct =
            model.finiteSpec.fsClauseType
    in
    div [ Attr.class "flex flex-wrap gap-2" ]
        [ pill (ct == Declarative) False (SetClauseType Declarative) "Declarative"
        , pill (ct == YesNoQuestion) False (SetClauseType YesNoQuestion) "Yes/No Q"
        , pill (isWhObject ct) False (SetClauseType (WhObject "what")) "Wh-Object"
        , pill (isWhAdjunct ct) False (SetClauseType (WhAdjunct "why")) "Wh-Adjunct"
        , pill (ct == WhSubject) False (SetClauseType WhSubject) "Wh-Subject"
        , pill (ct == TagQuestion) False (SetClauseType TagQuestion) "Tag Q"
        ]


isWhObject : ClauseType -> Bool
isWhObject ct =
    case ct of
        WhObject _ ->
            True

        _ ->
            False


isWhAdjunct : ClauseType -> Bool
isWhAdjunct ct =
    case ct of
        WhAdjunct _ ->
            True

        _ ->
            False


viewSubjectRow : Model -> Html Msg
viewSubjectRow model =
    div [ Attr.class "flex flex-wrap gap-2" ]
        (List.indexedMap
            (\idx ( label, _ ) ->
                pill (model.selectedSubjectIdx == idx) False (SetSubject idx) label
            )
            Lexicon.subjects
        )


viewVerbRow : Model -> Html Msg
viewVerbRow model =
    div [ Attr.class "flex flex-wrap gap-2" ]
        (List.indexedMap
            (\idx v ->
                pill (model.selectedVerbIdx == idx) False (SetVerb idx) (Lexicon.verbLabel v)
            )
            Lexicon.verbs
        )


viewArgsRow : Model -> Html Msg
viewArgsRow model =
    let
        trans =
            model.verb.vTrans

        voice =
            currentVoice model
    in
    div [ Attr.class "flex flex-wrap gap-3 items-end" ]
        (case ( trans, voice ) of
            ( Transitive, Active ) ->
                [ argInput "Object" (Maybe.map .npText model.arguments.argObj |> Maybe.withDefault "") SetArgObj ]

            ( Transitive, Passive ) ->
                [ argInput "By-agent" (Maybe.map .npText model.arguments.argBy |> Maybe.withDefault "") SetArgBy ]

            ( Ditransitive, Active ) ->
                [ argInput "Indirect obj" (Maybe.map .npText model.arguments.argIndObj |> Maybe.withDefault "") SetArgIndObj
                , argInput "Direct obj" (Maybe.map .npText model.arguments.argObj |> Maybe.withDefault "") SetArgObj
                ]

            ( Ditransitive, Passive ) ->
                [ argInput "Retained obj" (Maybe.map .npText model.arguments.argObj |> Maybe.withDefault "") SetArgObj
                , argInput "By-agent" (Maybe.map .npText model.arguments.argBy |> Maybe.withDefault "") SetArgBy
                ]

            ( Predicative, _ ) ->
                [ argInput "Complement" (model.arguments.argPred |> Maybe.withDefault "") SetArgPred ]

            ( SubcatClause _, _ ) ->
                [ argInput "Clause" (model.arguments.argClause |> Maybe.withDefault "") SetArgClause ]

            ( Intransitive, Passive ) ->
                [ argInput "By-agent" (Maybe.map .npText model.arguments.argBy |> Maybe.withDefault "") SetArgBy ]

            _ ->
                []
        )


argInput : String -> String -> (String -> Msg) -> Html Msg
argInput label val toMsg =
    div [ Attr.class "flex flex-col gap-1" ]
        [ span [ Attr.class "text-xs text-slate-500" ] [ text label ]
        , Html.input
            [ Attr.class "bg-slate-800 border border-slate-700 rounded-lg px-3 py-1.5 text-sm text-white focus:outline-none focus:border-indigo-500"
            , Attr.value val
            , Events.onInput toMsg
            ]
            []
        ]


viewParticipleFormRow : Model -> Html Msg
viewParticipleFormRow model =
    div [ Attr.class "flex flex-wrap gap-2" ]
        [ pill (model.participleForm == PresParticipleActive) False (SetParticipleForm PresParticipleActive) "Pres Active"
        , pill (model.participleForm == PresParticiplePassive) False (SetParticipleForm PresParticiplePassive) "Pres Passive"
        , pill (model.participleForm == PastParticiplePassive) False (SetParticipleForm PastParticiplePassive) "Past Passive"
        , pill (model.participleForm == PerfParticipleActive) False (SetParticipleForm PerfParticipleActive) "Perf Active"
        , pill (model.participleForm == PerfParticiplePassive) False (SetParticipleForm PerfParticiplePassive) "Perf Passive"
        ]



-- =========================================================
-- Pill button components
-- =========================================================


pill : Bool -> Bool -> Msg -> String -> Html Msg
pill selected disabled msg label =
    let
        baseClass =
            "px-3 py-1.5 rounded-full text-sm font-medium transition-colors cursor-pointer select-none"

        colorClass =
            if disabled then
                "bg-slate-800/50 text-slate-600 cursor-not-allowed"

            else if selected then
                "bg-indigo-600 text-white"

            else
                "bg-slate-800 text-slate-300 hover:bg-slate-700"

        attrs =
            if disabled then
                [ Attr.class (baseClass ++ " " ++ colorClass) ]

            else
                [ Attr.class (baseClass ++ " " ++ colorClass)
                , Events.onClick msg
                ]
    in
    span attrs [ text label ]


togglePill : Bool -> Bool -> Msg -> String -> Html Msg
togglePill active disabled msg label =
    pill active disabled msg label



-- =========================================================
-- Main
-- =========================================================


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
