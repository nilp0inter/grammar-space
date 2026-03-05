module Grammar.Engine exposing
    ( agreeNP
    , is3sg
    , modalWord
    , finiteDo
    , finiteHave
    , finiteBe
    , finiteLex
    , allAuxFinite
    , realizeFirstAux
    , linkForm
    , mainForm
    , contract
    , negateFirst
    , isInvertingClause
    , isObjectGapClause
    , activeComps
    , passiveComps
    , buildClauseParts
    , nullArgs
    , maybeToList
    )

import Grammar.Types exposing (..)


nullArgs : Arguments
nullArgs =
    Arguments Nothing Nothing Nothing Nothing Nothing


maybeToList : Maybe a -> List a
maybeToList m =
    case m of
        Just x ->
            [ x ]

        Nothing ->
            []


agreeNP : SubjectNP -> Arguments -> NP
agreeNP subj args =
    case subj of
        RefNP np ->
            np

        ExplIt ->
            NP "it" Third Singular

        ExplThere ->
            args.argObj
                |> Maybe.withDefault (NP "there" Third Singular)

        WhSubjNP w ->
            NP w Third Singular


is3sg : NP -> Bool
is3sg np =
    np.npPerson == Third && np.npNumber == Singular


modalWord : Modality -> String
modalWord m =
    case m of
        Can ->
            "can"

        Should ->
            "should"

        Must ->
            "must"

        Would ->
            "would"

        May ->
            "may"

        Might ->
            "might"

        Will ->
            "will"


finiteDo : Tense -> NP -> String
finiteDo t np =
    case t of
        Past ->
            "did"

        Present ->
            if is3sg np then
                "does"

            else
                "do"

        Future ->
            "do"


finiteHave : Tense -> NP -> String
finiteHave t np =
    case t of
        Past ->
            "had"

        Present ->
            if is3sg np then
                "has"

            else
                "have"

        Future ->
            "have"


finiteBe : Tense -> NP -> String
finiteBe t np =
    case t of
        Present ->
            if np.npPerson == First && np.npNumber == Singular then
                "am"

            else if is3sg np then
                "is"

            else
                "are"

        Past ->
            if np.npPerson == Second || np.npNumber == Plural then
                "were"

            else
                "was"

        Future ->
            "be"


finiteLex : Tense -> NP -> Verb -> String
finiteLex t np v =
    case t of
        Past ->
            v.vPast

        Present ->
            if is3sg np then
                v.v3sg

            else
                v.vBase

        Future ->
            v.vBase


allAuxFinite : FiniteSpec -> List Aux
allAuxFinite s =
    let
        m =
            case s.fsModal of
                Just md ->
                    [ AModal (modalWord md) ]

                Nothing ->
                    if s.fsTense == Future then
                        [ AModal "will" ]

                    else
                        []

        struct =
            (if s.fsPerfect then
                [ AHave ]

             else
                []
            )
                ++ (if s.fsProgressive then
                        [ ABe ]

                    else
                        []
                   )
                ++ (if s.fsVoice == Passive then
                        [ ABe ]

                    else
                        []
                   )
    in
    m ++ struct


realizeFirstAux : Tense -> NP -> Aux -> String
realizeFirstAux t np aux =
    case aux of
        AModal w ->
            w

        AHave ->
            finiteHave t np

        ABe ->
            finiteBe t np

        ADo ->
            finiteDo t np


linkForm : Aux -> Aux -> String
linkForm prev next =
    case ( prev, next ) of
        ( AModal _, AHave ) ->
            "have"

        ( AModal _, ABe ) ->
            "be"

        ( AHave, ABe ) ->
            "been"

        ( ABe, ABe ) ->
            "being"

        _ ->
            ""


mainForm : Bool -> Bool -> Voice -> Verb -> String
mainForm perfect progressive voice v =
    if voice == Passive then
        v.vPastPart

    else if progressive then
        v.vPresPart

    else if perfect then
        v.vPastPart

    else
        v.vBase


contract : String -> String
contract w =
    case w of
        "is" ->
            "isn't"

        "are" ->
            "aren't"

        "was" ->
            "wasn't"

        "were" ->
            "weren't"

        "has" ->
            "hasn't"

        "have" ->
            "haven't"

        "had" ->
            "hadn't"

        "does" ->
            "doesn't"

        "do" ->
            "don't"

        "did" ->
            "didn't"

        "can" ->
            "can't"

        "will" ->
            "won't"

        "should" ->
            "shouldn't"

        "would" ->
            "wouldn't"

        "must" ->
            "mustn't"

        "might" ->
            "mightn't"

        "may" ->
            "may not"

        _ ->
            w ++ " not"


negateFirst : Style -> String -> List String
negateFirst style a =
    case style of
        Full ->
            [ a, "not" ]

        Contracted ->
            if a == "am" then
                [ "am", "not" ]

            else
                String.words (contract a)


isInvertingClause : ClauseType -> Bool
isInvertingClause ct =
    case ct of
        YesNoQuestion ->
            True

        WhObject _ ->
            True

        WhAdjunct _ ->
            True

        _ ->
            False


isObjectGapClause : ClauseType -> Bool
isObjectGapClause ct =
    case ct of
        WhObject _ ->
            True

        _ ->
            False


activeComps : Bool -> Verb -> Arguments -> List String
activeComps skipObj v args =
    case v.vTrans of
        Intransitive ->
            []

        Transitive ->
            if skipObj then
                []

            else
                maybeToList (Maybe.map .npText args.argObj)

        Ditransitive ->
            maybeToList (Maybe.map .npText args.argIndObj)
                ++ (if skipObj then
                        []

                    else
                        maybeToList (Maybe.map .npText args.argObj)
                   )

        Predicative ->
            maybeToList args.argPred

        SubcatClause _ ->
            maybeToList args.argClause


passiveComps : Bool -> Verb -> Arguments -> List String
passiveComps skipObj v args =
    let
        retainedObj =
            if v.vTrans == Ditransitive && not skipObj then
                maybeToList (Maybe.map .npText args.argObj)

            else
                []

        byPhrase =
            case args.argBy of
                Just b ->
                    [ "by", b.npText ]

                Nothing ->
                    []
    in
    retainedObj ++ byPhrase


buildClauseParts : Style -> SubjectNP -> Verb -> Arguments -> FiniteSpec -> Result String ClauseParts
buildClauseParts style subj v args s =
    let
        objGap =
            isObjectGapClause s.fsClauseType
    in
    checkProgressive v s
        |> Result.andThen (\_ -> checkPassive v s)
        |> Result.andThen (\_ -> checkActiveArgs objGap v args s)
        |> Result.andThen (\_ -> buildParts style subj v args s objGap)


checkProgressive : Verb -> FiniteSpec -> Result String ()
checkProgressive v s =
    if s.fsProgressive && not v.vAllowsProg then
        Err "Verb blocks progressive aspect."

    else
        Ok ()


checkPassive : Verb -> FiniteSpec -> Result String ()
checkPassive v s =
    if s.fsVoice == Passive && (v.vTrans == Intransitive || v.vTrans == Predicative) then
        Err "Intransitive/predicative verbs cannot be passivized."

    else
        Ok ()


checkActiveArgs : Bool -> Verb -> Arguments -> FiniteSpec -> Result String ()
checkActiveArgs objGap v args s =
    if s.fsVoice == Active then
        case v.vTrans of
            Transitive ->
                if not objGap && args.argObj == Nothing then
                    Err "Transitive active clause requires an object."

                else
                    Ok ()

            Predicative ->
                if args.argPred == Nothing then
                    Err "Missing complement."

                else
                    Ok ()

            Ditransitive ->
                if args.argIndObj == Nothing || (not objGap && args.argObj == Nothing) then
                    Err "Missing ditransitive objects."

                else
                    Ok ()

            SubcatClause _ ->
                if args.argClause == Nothing then
                    Err "Missing clausal complement."

                else
                    Ok ()

            Intransitive ->
                if args.argObj /= Nothing then
                    Err "Intransitive verb cannot take a direct object."

                else
                    Ok ()

    else
        Ok ()


buildParts : Style -> SubjectNP -> Verb -> Arguments -> FiniteSpec -> Bool -> Result String ClauseParts
buildParts style subj v args s objGap =
    let
        npSubj =
            agreeNP subj args

        baseAux =
            allAuxFinite s

        requiresInversion =
            isInvertingClause s.fsClauseType

        ( surfaceAux, isMainEmpty ) =
            if List.isEmpty baseAux then
                if v.vClass == Copular then
                    ( [ ABe ], True )

                else if s.fsPolarity == Negative || requiresInversion then
                    ( [ ADo ], False )

                else
                    ( [], False )

            else
                ( baseAux, False )

        processRest prev rest =
            case rest of
                [] ->
                    []

                x :: xs ->
                    linkForm prev x :: processRest x xs

        ( aux1T, negT, auxR ) =
            case surfaceAux of
                [] ->
                    ( Nothing, [], [] )

                a :: as_ ->
                    let
                        fw =
                            realizeFirstAux s.fsTense npSubj a

                        rw =
                            processRest a as_

                        ( finalFw, finalNeg ) =
                            if s.fsPolarity == Negative then
                                case style of
                                    Full ->
                                        ( fw, [ "not" ] )

                                    Contracted ->
                                        if fw == "am" then
                                            ( fw, [ "not" ] )

                                        else
                                            case String.words (contract fw) of
                                                [] ->
                                                    ( fw, [ "not" ] )

                                                x :: xs ->
                                                    ( x, xs )

                            else
                                ( fw, [] )
                    in
                    ( Just finalFw, finalNeg, rw )

        mainOpt =
            if isMainEmpty then
                Nothing

            else
                Just
                    (case surfaceAux of
                        [] ->
                            finiteLex s.fsTense npSubj v

                        (ADo :: _) ->
                            v.vBase

                        _ ->
                            mainForm s.fsPerfect s.fsProgressive s.fsVoice v
                    )

        skipObj =
            isObjectGapClause s.fsClauseType

        comps =
            case s.fsVoice of
                Passive ->
                    passiveComps skipObj v args

                Active ->
                    activeComps skipObj v args

        subjText =
            case subj of
                WhSubjNP w ->
                    w

                ExplIt ->
                    "it"

                ExplThere ->
                    "there"

                RefNP np ->
                    np.npText
    in
    Ok (ClauseParts subjText aux1T negT auxR mainOpt comps)
