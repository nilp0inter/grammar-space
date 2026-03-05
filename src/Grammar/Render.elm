module Grammar.Render exposing
    ( renderFinite
    , renderFiniteWords
    , renderEmbedded
    , renderEmbeddedWords
    , renderRelative
    , renderRelativeWords
    , renderImperative
    , renderImperativeWords
    , renderNonFinite
    , renderNonFiniteWords
    , compWord
    , relWord
    , tagPronoun
    )

import Grammar.Engine exposing (..)
import Grammar.Types exposing (..)



-- Helper: convert Maybe to list of tagged words


tagMaybe : WordRole -> Maybe String -> List SentenceWord
tagMaybe role m =
    case m of
        Just s ->
            [ SentenceWord s role ]

        Nothing ->
            []


tagList : WordRole -> List String -> List SentenceWord
tagList role strs =
    List.map (\s -> SentenceWord s role) strs



-- Tag question helpers


tagPronoun : SubjectNP -> String
tagPronoun subj =
    case subj of
        RefNP np ->
            if np.npNumber == Plural then
                "they"

            else if np.npPerson == First then
                "I"

            else if np.npPerson == Second then
                "you"

            else
                "it"

        ExplIt ->
            "it"

        ExplThere ->
            "there"

        WhSubjNP _ ->
            "it"


compWord : Complementizer -> String
compWord c =
    case c of
        ThatC ->
            "that"

        IfC ->
            "if"

        WhetherC ->
            "whether"

        BecauseC ->
            "because"


relWord : RelPron -> String
relWord r =
    case r of
        ThatRel ->
            "that"

        WhichRel ->
            "which"

        WhoRel ->
            "who"



-- =========================================================
-- Finite clauses (word-level)
-- =========================================================


renderFiniteWords : Style -> SubjectNP -> Verb -> Arguments -> FiniteSpec -> Result String (List SentenceWord)
renderFiniteWords style subj v args s =
    buildClauseParts style subj v args s
        |> Result.map
            (\cp ->
                let
                    punct =
                        case s.fsClauseType of
                            Declarative ->
                                "."

                            _ ->
                                "?"

                    strWords =
                        case s.fsClauseType of
                            Declarative ->
                                tagList SubjectWord [ cp.cpSubj ]
                                    ++ tagMaybe Aux1Word cp.cpAux1
                                    ++ tagList NegWord cp.cpNeg
                                    ++ tagList AuxRWord cp.cpAuxR
                                    ++ tagMaybe MainVerbWord cp.cpMain
                                    ++ tagList ComplementWord cp.cpComp

                            YesNoQuestion ->
                                tagMaybe Aux1Word cp.cpAux1
                                    ++ tagList SubjectWord [ cp.cpSubj ]
                                    ++ tagList NegWord cp.cpNeg
                                    ++ tagList AuxRWord cp.cpAuxR
                                    ++ tagMaybe MainVerbWord cp.cpMain
                                    ++ tagList ComplementWord cp.cpComp

                            WhObject wh ->
                                tagList WhWord [ wh ]
                                    ++ tagMaybe Aux1Word cp.cpAux1
                                    ++ tagList SubjectWord [ cp.cpSubj ]
                                    ++ tagList NegWord cp.cpNeg
                                    ++ tagList AuxRWord cp.cpAuxR
                                    ++ tagMaybe MainVerbWord cp.cpMain
                                    ++ tagList ComplementWord cp.cpComp

                            WhAdjunct wh ->
                                tagList WhWord [ wh ]
                                    ++ tagMaybe Aux1Word cp.cpAux1
                                    ++ tagList SubjectWord [ cp.cpSubj ]
                                    ++ tagList NegWord cp.cpNeg
                                    ++ tagList AuxRWord cp.cpAuxR
                                    ++ tagMaybe MainVerbWord cp.cpMain
                                    ++ tagList ComplementWord cp.cpComp

                            WhSubject ->
                                tagList SubjectWord [ cp.cpSubj ]
                                    ++ tagMaybe Aux1Word cp.cpAux1
                                    ++ tagList NegWord cp.cpNeg
                                    ++ tagList AuxRWord cp.cpAuxR
                                    ++ tagMaybe MainVerbWord cp.cpMain
                                    ++ tagList ComplementWord cp.cpComp

                            TagQuestion ->
                                let
                                    body =
                                        tagList SubjectWord [ cp.cpSubj ]
                                            ++ tagMaybe Aux1Word cp.cpAux1
                                            ++ tagList NegWord cp.cpNeg
                                            ++ tagList AuxRWord cp.cpAuxR
                                            ++ tagMaybe MainVerbWord cp.cpMain
                                            ++ tagList ComplementWord cp.cpComp

                                    oppPol =
                                        if s.fsPolarity == Affirmative then
                                            Negative

                                        else
                                            Affirmative

                                    tagAuxBase =
                                        case allAuxFinite s of
                                            a :: _ ->
                                                a

                                            [] ->
                                                if v.vClass == Copular then
                                                    ABe

                                                else
                                                    ADo

                                    npSubj =
                                        agreeNP subj args

                                    tagWBase =
                                        realizeFirstAux s.fsTense npSubj tagAuxBase

                                    tagAuxFinal =
                                        if oppPol == Negative then
                                            if s.fsTense == Present && tagWBase == "am" then
                                                "aren't"

                                            else
                                                contract tagWBase

                                        else
                                            tagWBase
                                in
                                body
                                    ++ [ SentenceWord "," PunctuationWord
                                       , SentenceWord tagAuxFinal TagWord
                                       , SentenceWord (tagPronoun subj) TagWord
                                       ]
                in
                strWords ++ [ SentenceWord punct PunctuationWord ]
            )


renderFinite : Style -> SubjectNP -> Verb -> Arguments -> FiniteSpec -> Result String String
renderFinite style subj v args s =
    renderFiniteWords style subj v args s
        |> Result.map wordsToString



-- =========================================================
-- Embedded clauses
-- =========================================================


renderEmbeddedWords : Style -> Complementizer -> SubjectNP -> Verb -> Arguments -> FiniteSpec -> Result String (List SentenceWord)
renderEmbeddedWords style c subj v args s =
    buildClauseParts style subj v args { s | fsClauseType = Declarative }
        |> Result.map
            (\cp ->
                [ SentenceWord (compWord c) ComplementizerWord ]
                    ++ tagList SubjectWord [ cp.cpSubj ]
                    ++ tagMaybe Aux1Word cp.cpAux1
                    ++ tagList NegWord cp.cpNeg
                    ++ tagList AuxRWord cp.cpAuxR
                    ++ tagMaybe MainVerbWord cp.cpMain
                    ++ tagList ComplementWord cp.cpComp
            )


renderEmbedded : Style -> Complementizer -> SubjectNP -> Verb -> Arguments -> FiniteSpec -> Result String String
renderEmbedded style c subj v args s =
    renderEmbeddedWords style c subj v args s
        |> Result.map wordsToString



-- =========================================================
-- Relative clauses
-- =========================================================


renderRelativeWords : Style -> RelPron -> RelGap -> SubjectNP -> Verb -> Arguments -> FiniteSpec -> Result String (List SentenceWord)
renderRelativeWords style rp gap subj v args s =
    let
        s2 =
            case gap of
                RelObj ->
                    { s | fsClauseType = WhObject "" }

                RelSubj ->
                    { s | fsClauseType = Declarative }
    in
    buildClauseParts style subj v args s2
        |> Result.map
            (\cp ->
                let
                    body =
                        case gap of
                            RelObj ->
                                tagList SubjectWord [ cp.cpSubj ]
                                    ++ tagMaybe Aux1Word cp.cpAux1
                                    ++ tagList NegWord cp.cpNeg
                                    ++ tagList AuxRWord cp.cpAuxR
                                    ++ tagMaybe MainVerbWord cp.cpMain
                                    ++ tagList ComplementWord cp.cpComp

                            RelSubj ->
                                tagMaybe Aux1Word cp.cpAux1
                                    ++ tagList NegWord cp.cpNeg
                                    ++ tagList AuxRWord cp.cpAuxR
                                    ++ tagMaybe MainVerbWord cp.cpMain
                                    ++ tagList ComplementWord cp.cpComp
                in
                List.filter (\w -> w.text /= "")
                    (SentenceWord (relWord rp) RelWord :: body)
            )


renderRelative : Style -> RelPron -> RelGap -> SubjectNP -> Verb -> Arguments -> FiniteSpec -> Result String String
renderRelative style rp gap subj v args s =
    renderRelativeWords style rp gap subj v args s
        |> Result.map wordsToString



-- =========================================================
-- Imperative
-- =========================================================


renderImperativeWords : Style -> Verb -> Arguments -> ImperativeSpec -> Result String (List SentenceWord)
renderImperativeWords style v args s =
    let
        checkProg =
            if s.isProgressive && not v.vAllowsProg then
                Err "Verb blocks progressive aspect."

            else
                Ok ()

        checkPass =
            if s.isVoice == Passive && (v.vTrans == Intransitive || v.vTrans == Predicative) then
                Err "Intransitive/predicative verbs cannot be passivized."

            else
                Ok ()
    in
    checkProg
        |> Result.andThen (\_ -> checkPass)
        |> Result.andThen (\_ -> buildImperativeWords style v args s)


buildImperativeWords : Style -> Verb -> Arguments -> ImperativeSpec -> Result String (List SentenceWord)
buildImperativeWords style v args s =
    let
        baseAux =
            (if s.isPerfect then
                [ AHave ]

             else
                []
            )
                ++ (if s.isProgressive then
                        [ ABe ]

                    else
                        []
                   )
                ++ (if s.isVoice == Passive then
                        [ ABe ]

                    else
                        []
                   )

        processRest prev rest =
            case rest of
                [] ->
                    []

                x :: xs ->
                    linkForm prev x :: processRest x xs

        realizeBareFirst aux =
            case aux of
                AHave ->
                    "have"

                ABe ->
                    "be"

                _ ->
                    ""

        auxStrs =
            case baseAux of
                [] ->
                    []

                a :: as_ ->
                    realizeBareFirst a :: processRest a as_

        mainW =
            mainForm s.isPerfect s.isProgressive s.isVoice v

        comps =
            case s.isVoice of
                Passive ->
                    passiveComps False v args

                Active ->
                    activeComps False v args

        affirmativeWords =
            if List.isEmpty auxStrs then
                tagList MainVerbWord [ v.vBase ]
                    ++ tagList ComplementWord comps
                    ++ [ SentenceWord "!" PunctuationWord ]

            else
                tagList AuxRWord auxStrs
                    ++ tagList MainVerbWord [ mainW ]
                    ++ tagList ComplementWord comps
                    ++ [ SentenceWord "!" PunctuationWord ]

        negWords =
            let
                negParts =
                    negateFirst style "do"
            in
            if List.isEmpty auxStrs then
                tagNegParts negParts
                    ++ tagList MainVerbWord [ v.vBase ]
                    ++ tagList ComplementWord comps
                    ++ [ SentenceWord "!" PunctuationWord ]

            else
                tagNegParts negParts
                    ++ tagList AuxRWord auxStrs
                    ++ tagList MainVerbWord [ mainW ]
                    ++ tagList ComplementWord comps
                    ++ [ SentenceWord "!" PunctuationWord ]

        tagNegParts parts =
            case parts of
                [] ->
                    []

                first :: rest ->
                    SentenceWord first Aux1Word :: tagList NegWord rest
    in
    Ok
        (case s.isPolarity of
            Affirmative ->
                affirmativeWords

            Negative ->
                negWords
        )


renderImperative : Style -> Verb -> Arguments -> ImperativeSpec -> Result String String
renderImperative style v args s =
    renderImperativeWords style v args s
        |> Result.map wordsToString



-- =========================================================
-- Non-finite
-- =========================================================


renderNonFiniteWords : Verb -> Arguments -> NonFinite -> Result String (List SentenceWord)
renderNonFiniteWords v args nf =
    case nf of
        Infinitive r ->
            renderInfinitiveWords v args r.perfect r.progressive r.voice r.polarity

        Gerund r ->
            renderGerundWords v args r.perfect r.voice r.polarity

        Participle r ->
            renderParticipleWords v args r.form r.polarity


renderInfinitiveWords : Verb -> Arguments -> Bool -> Bool -> Voice -> Polarity -> Result String (List SentenceWord)
renderInfinitiveWords v args perfect progressive voice pol =
    let
        checkProg =
            if progressive && not v.vAllowsProg then
                Err "Verb blocks progressive aspect."

            else
                Ok ()

        checkPass =
            if voice == Passive && (v.vTrans == Intransitive || v.vTrans == Predicative) then
                Err "Intransitive/predicative verbs cannot be passivized."

            else
                Ok ()
    in
    checkProg
        |> Result.andThen (\_ -> checkPass)
        |> Result.andThen
            (\_ ->
                let
                    baseAux =
                        (if perfect then
                            [ AHave ]

                         else
                            []
                        )
                            ++ (if progressive then
                                    [ ABe ]

                                else
                                    []
                               )
                            ++ (if voice == Passive then
                                    [ ABe ]

                                else
                                    []
                               )

                    processRest prev rest =
                        case rest of
                            [] ->
                                []

                            x :: xs ->
                                linkForm prev x :: processRest x xs

                    bareFirst aux =
                        case aux of
                            AHave ->
                                "have"

                            ABe ->
                                "be"

                            _ ->
                                ""

                    auxStrs =
                        case baseAux of
                            [] ->
                                []

                            a :: as_ ->
                                bareFirst a :: processRest a as_

                    mainW =
                        mainForm perfect progressive voice v

                    comps =
                        case voice of
                            Passive ->
                                passiveComps False v args

                            Active ->
                                activeComps False v args

                    core =
                        List.filter (\w -> w.text /= "")
                            (tagList AuxRWord [ "to" ]
                                ++ tagList AuxRWord auxStrs
                                ++ tagList MainVerbWord [ mainW ]
                                ++ tagList ComplementWord comps
                            )
                in
                Ok
                    (case pol of
                        Affirmative ->
                            core

                        Negative ->
                            SentenceWord "not" NegWord :: core
                    )
            )


renderGerundWords : Verb -> Arguments -> Bool -> Voice -> Polarity -> Result String (List SentenceWord)
renderGerundWords v args perfect voice pol =
    let
        checkPass =
            if voice == Passive && (v.vTrans == Intransitive || v.vTrans == Predicative) then
                Err "Intransitive/predicative verbs cannot be passivized."

            else
                Ok ()
    in
    checkPass
        |> Result.andThen
            (\_ ->
                let
                    coreStrs =
                        case ( perfect, voice ) of
                            ( False, Active ) ->
                                [ v.vPresPart ]

                            ( False, Passive ) ->
                                [ "being", v.vPastPart ]

                            ( True, Active ) ->
                                [ "having", v.vPastPart ]

                            ( True, Passive ) ->
                                [ "having", "been", v.vPastPart ]

                    comps =
                        case voice of
                            Active ->
                                activeComps False v args

                            Passive ->
                                passiveComps False v args

                    coreWords =
                        tagList MainVerbWord coreStrs
                            ++ tagList ComplementWord comps
                in
                Ok
                    (if pol == Negative then
                        SentenceWord "not" NegWord :: coreWords

                     else
                        coreWords
                    )
            )


renderParticipleWords : Verb -> Arguments -> ParticipleForm -> Polarity -> Result String (List SentenceWord)
renderParticipleWords v args pf pol =
    let
        voice =
            participleVoice pf

        checkPass =
            if voice == Passive && (v.vTrans == Intransitive || v.vTrans == Predicative) then
                Err "Intransitive/predicative verbs cannot be passivized."

            else
                Ok ()
    in
    checkPass
        |> Result.andThen
            (\_ ->
                let
                    coreStrs =
                        case pf of
                            PresParticipleActive ->
                                [ v.vPresPart ]

                            PresParticiplePassive ->
                                [ "being", v.vPastPart ]

                            PastParticiplePassive ->
                                [ v.vPastPart ]

                            PerfParticipleActive ->
                                [ "having", v.vPastPart ]

                            PerfParticiplePassive ->
                                [ "having", "been", v.vPastPart ]

                    comps =
                        case voice of
                            Active ->
                                activeComps False v args

                            Passive ->
                                passiveComps False v args

                    coreWords =
                        tagList MainVerbWord coreStrs
                            ++ tagList ComplementWord comps
                in
                Ok
                    (if pol == Negative then
                        SentenceWord "not" NegWord :: coreWords

                     else
                        coreWords
                    )
            )


participleVoice : ParticipleForm -> Voice
participleVoice pf =
    case pf of
        PresParticipleActive ->
            Active

        PresParticiplePassive ->
            Passive

        PastParticiplePassive ->
            Passive

        PerfParticipleActive ->
            Active

        PerfParticiplePassive ->
            Passive


renderNonFinite : Verb -> Arguments -> NonFinite -> Result String String
renderNonFinite v args nf =
    renderNonFiniteWords v args nf
        |> Result.map wordsToString



-- =========================================================
-- String rendering helper
-- =========================================================


wordsToString : List SentenceWord -> String
wordsToString words =
    let
        addWord w acc =
            case acc of
                "" ->
                    w.text

                _ ->
                    if w.role == PunctuationWord && w.text /= "(" then
                        if w.text == "," || w.text == "." || w.text == "?" || w.text == "!" then
                            acc ++ w.text

                        else
                            acc ++ " " ++ w.text

                    else
                        acc ++ " " ++ w.text
    in
    List.foldl addWord "" words
